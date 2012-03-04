#!/usr/bin/env tclsh
# Wibble - a pure-Tcl Web server.  http://wiki.tcl.tk/23626
# Copyright 2010 Andy Goth.  mailto/andrew.m.goth/at/gmail/dot/com
# Available under the Tcl/Tk license.  http://tcl.tk/software/tcltk/license.html

package require Tcl 8.6
package provide wibble 0.1

# Define the wibble namespace.
namespace eval wibble {
    variable zonehandlers
}

# ============================== zone handlers ================================

# Echo request dictionary.
proc wibble::vars {state} {
    dict set response status 200
    dict set response header content-type text/html
    dict set response content "<html><head><style type=\"text/css\">\
        body {font-family: monospace}\
        table {border-collapse: collapse; outline: 1px solid #000}\
        th {white-space: nowrap; text-align: left}\
        th, td {border: 1px solid #727772}\
        tr:nth-child(odd) {background-color: #ded}\
        tr:nth-child(even) {background-color: #eee}\
        th.title {background-color: #8d958d; text-align: center}\
        </style></head><body><table>"
    foreach {dictname dictval} $state {
        if {$dictname eq "request"} {
            set dictval [dumprequest $dictval]
        }
        dict append response content\
            "<tr><th class=\"title\" colspan=\"2\">[enhtml $dictname]</th></tr>"
        foreach {key val} $dictval {
            dict append response content\
                <tr><th>[enhtml $key]</th><td>[enhtml $val]</td></tr>
        }
    }
    dict append response content </table></body></html>
    sendresponse $response
}

# Redirect when a directory is requested without a trailing slash.
proc wibble::dirslash {state} {
    dict with state request {}; dict with state options {}
    if {[file isdirectory $fspath] && [string index $suffix end] ni {/ ""}} {
        dict set response status 301
        dict set response header location $path/$rawquery
        sendresponse $response
    }
}

# Rewrite directory requests to search for an indexfile.
proc wibble::indexfile {state} {
    dict with state request {}; dict with state options {}
    if {[file isdirectory $fspath]} {
        if {[string index $path end] ne "/"} {
            append path /
        }
        set newstate $state
        dict set state request path $path$indexfile
        nexthandler $newstate $state
    }
}

# Generate directory listings.
proc wibble::dirlist {state} {
    dict with state request {}; dict with state options {}
    if {![file isdirectory $fspath]} {
        # Pass if the requested object is not a directory or doesn't exist.
    } elseif {[file readable $fspath]} {
        # If the directory is readable, generate a listing.
        dict set response status 200
        dict set response header content-type text/html
        dict set response content <html><body>
        dict append response content "<a href=\"..\">..</a><br />\n"
        foreach elem [lsort [glob -nocomplain -tails -directory $fspath *]] {
            dict append response content\
                "<a href=\"[enurl $elem]\">[enhtml $elem]</a><br />\n"
        }
        dict append response content </body></html>\n
        sendresponse $response
    } else {
        # But if it isn't readable, generate a 403.
        dict set response status 403
        dict set response header content-type text/plain
        dict set response content Forbidden\n
        sendresponse $response
    }
}

# Compile templates into scripts.
proc wibble::template {state} {
    dict with state request {}; dict with state options {}
    if {[file readable $fspath.tmpl] && (![file readable $fspath.script]
      || [file mtime $fspath.script] < [file mtime $fspath.tmpl])} {
        set chan [open $fspath.tmpl]
        set tmpl [read $chan]
        chan close $chan
        set chan [open $fspath.script w]
        chan puts -nonewline $chan\
            [compiletemplate "dict append response content" $tmpl]
        chan close $chan
    }
}

# Execute scripts.
proc wibble::script {state} {
    dict with state request {}; dict with state options {}
    if {[file readable $fspath.script]} {
        dict set response status 200
        dict set response header content-type text/plain
        dict set response content ""
        source $fspath.script
        sendresponse $response
    }
}

# Send static files.
proc wibble::static {state} {
    dict with state request {}; dict with state options {}
    if {![file isdirectory $fspath] && [file exists $fspath]} {
        dict set response status 200
        dict set response contentfile $fspath
        sendresponse $response
    }
}

# Send a 404.
proc wibble::notfound {state} {
    dict set response status 404
    dict set response header content-type text/plain
    dict set response content "not found: [dict get $state request uri]\n"
    sendresponse $response
}

# ============================ utility procedures =============================

# Compile a template.
proc wibble::compiletemplate {command template} {
    set script ""
    set pos 0
    foreach match [regexp -line -all -inline -indices {^%.*$} $template] {
        lassign $match from to
        set str [string range $template $pos [expr {$from - 2}]]
        if {$str ne ""} {
            append script "$command \[[list subst $str\n]\]\n"
        }
        append script [string range $template [expr {$from + 1}] $to]\n
        set pos [expr {$to + 2}]
    }
    set str [string range $template $pos end]
    if {$str ne ""} {
        append script "$command \[[list subst $str]\]"
    }
    return $script
}

# Flatten the request dictionary into a form that's easier to log.
proc wibble::dumprequest {data {prefix ""}} {
    if {![llength $data]} {
        return [list $prefix ""]
    }
    set result {}
    foreach {key val} $data {
        set key [concat $prefix [list $key]]
        if {$key in {header "header content-type" "header cookie" accept query}
         || ([lindex $key 0] in {post query} && ([llength $key] < 3
          || ([llength $key] == 3 && [lindex $key 2] ne "")))} {
            lappend result {*}[dumprequest $val $key]
        } elseif {[string length $val] > 512 || [string first \n $val] != -1} {
            lappend result $key (len=[string length $val])
        } else {
            lappend result $key $val
        }
    }
    return $result
}

# ================== coroutine and network input procedures ===================

# Construct a command to resume the current coroutine and pass it event data.
proc wibble::resume {args} {
    set lambda [list {coro args} {$coro $args} [namespace current]]
    concat [list apply $lambda [info coroutine]] $args
}

# Suspend the current coroutine until one of the requested events occurs.
proc wibble::suspend {args} {
    # Install a readable handler only if readability is a requested event.
    if {"readable" in $args} {
        set chan [namespace tail [info coroutine]]
        chan event $chan readable [resume readable]
    }

    # Yield until a requested event happens.
    set event [yield]
    while {[lindex $event 0] ni $args} {
        set event [yield]
    }

    # Remove the readability handler if it was installed.
    if {"readable" in $args} {
        chan event $chan readable ""
    }

    # Return the event type and any extra data that was passed to [resume].
    return $event
}

# Get a line of data from a channel.
proc wibble::getline {chan} {
    while {1} {
        if {[chan gets $chan line] >= 0} {
            return $line
        } elseif {[chan eof $chan]} {
            return -level [info level]
        } elseif {[chan pending input $chan] > 4096} {
            error "line length exceeds limit of 4096 bytes"
        }
        suspend readable
    }
}

# Get a block of data from a channel.
proc wibble::getblock {chan size} {
    while {1} {
        set chunklet [chan read $chan $size]
        set size [expr {$size - [string length $chunklet]}]
        append chunk $chunklet
        if {[chan eof $chan]} {
            return -level [info level]
        } elseif {$size == 0} {
            return $chunk
        }
        suspend readable
    }
}

# ==================== conversion and parsing procedures ======================

# Encode for HTML by substituting angle brackets, ampersands, line breaks, and
# space sequences.
proc wibble::enhtml {str} {
    string map {< &lt; > &gt; & &amp; \n "<br />\n" "  " " &\#160;"} $str
}

# Encode for HTML tag attribute by substituting angle brackets, ampersands,
# double quotes, and space sequences.
proc wibble::enattr {str} {
    string map {< &lt; > &gt; & &amp; \" &quot; "  " " &\#160;"} $str
}

# Encode for HTML <pre> by substituting angle brackets and ampersands.
proc wibble::enpre {str} {
    string map {< &lt; > &gt; & &amp;} $str
}

# Encode a query string.  The caller must prepend the question mark.
proc wibble::enquery {args} {
    set query {}
    foreach {key val} [concat {*}$args] {
        if {[dict exists $val ""]} {
            lappend query [enurl $key]=[enurl [dict get $val ""]]
        } else {
            lappend query [enurl $key]
        }
    }
    join $query &
}

# Decode a query string into a list.  The caller must strip the question mark.
proc wibble::dequery {str} {
    set query {}
    foreach elem [split $str &] {
        regexp {^([^=]*)(?:(=.*))?$} $elem _ key val
        if {$val ne ""} {
            set val [list "" [deurl [string range $val 1 end]]]
        }
        lappend query [deurl $key] $val
    }
    return $query
}

# Encode by substituting most non-alphanumerics with hexadecimal codes.
proc wibble::enhex {str} {
    set pos 0
    while {[regexp -indices -start $pos {[^-^,./'=+|!$\w]} $str range]} {
        binary scan [string range $str {*}$range] H2 char
        set str [string replace $str {*}$range %$char]
        set pos [expr {[lindex $range 0] + 3}]
    }
    return $str
}

# Decode hexadecimal encoding.
proc wibble::dehex {str} {
    set pos 0
    while {[regexp -indices -start $pos {%([[:xdigit:]]{2})} $str range code]} {
        set char [binary format H2 [string range $str {*}$code]]
        set str [string replace $str {*}$range $char]
        set pos [expr {[lindex $range 0] + 1}]
    }
    return $str
}

# Encode for URLs by substituting plus, space, and most other non-alphanumerics.
proc wibble::enurl {str} {
    enhex [string map {+ %2b " " +} $str]
}

# Decode URL encoding.
proc wibble::deurl {str} {
    dehex [string map {+ " "} $str]
}

# Decode header list encoding.
proc wibble::delist {separator str} {
    regexp -all -inline [dict get {
        semicolon {"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^;]+}
        comma     {"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^,]+}
        semicomma {"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^,;]+}
        space     {"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^"()\\\s]+}
    } $separator] $str
}

# Decode header quoting.
proc wibble::dequote {str} {
    if {([string index $str 0] eq "\"" && [string index $str end] eq "\"")
     || ([string index $str 0] eq "(" && [string index $str end] eq ")")} {
        regsub -all {\\(.)} [string range $str 1 end-1] {\1}
    } else {
        return $str
    }
}

# Decode headers.
proc wibble::deheader {str} {
    set header {}
    foreach {_ key raw} [regexp -all -inline -expanded -lineanchor {
        ^( [^\s:]+ ) \s*:\s*
        ( (?: "(?:[^\\"]|\\.)*" | \((?:[^\\()]|\\.)*\) | [^\n] | \n[ \t] )* )
    } $str] {
        set key [string tolower $key]
        set raw [string trim $raw]
        set val {}
        switch $key {
        cookie {
            # Value is a cookie definition.
            set common {}
            set cookie ""
            foreach elem [delist semicomma $raw] {
                regexp {\s*([^\s=]*)(?:\s*=(.*))?} $elem _ key2 val2
                set key2 [string tolower $key2]
                if {[string index $key2 0] eq "\$"} {
                    set key2 [dehex [string range $key2 1 end]]
                    if {$cookie eq ""} {
                        dict set common $key2 [dehex $val2]
                    } else {
                        dict set params $key2 [dehex $val2]
                    }
                } else {
                    if {$cookie ne ""} {
                        lappend val $cookie $params
                    }
                    set cookie [dehex $key2]
                    set params $common
                    dict set params "" [dehex $val2]
                }
            }
            if {$cookie ne ""} {
                lappend val $cookie $params
            }
        } cache-control - pragma {
            # Value has format "subkey1=subval1,subkey2=subval2".
            foreach elem [delist comma $raw] {
                regexp {\s*([^\s=]+)(?:\s*(=.*))?} $elem _ key2 val2
                if {$val2 ne ""} {
                    set val2 [dequote [string trim [string range $val2 1 end]]]
                    set val2 [list "" $val2]
                }
                lappend val [string tolower $key2] $val2
            }
        } connection - content-encoding - content-language - if-match -
        if-none-match - trailer - upgrade - vary - via - warning {
            # Value has format "elem1,elem2".
            foreach elem [delist comma $raw] {
                lappend val [dequote [string trim $elem]]
            }
        } accept - accept-charset - accept-encoding - accept-language -
        expect - te - transfer-encoding {
            # Value has format "elem1;subkey1=subval1;subkey2=subval2,elem2".
            foreach elem [delist comma $raw] {
                set params {}
                set subs [delist semicolon $elem]
                foreach sub [lrange $subs 1 end] {
                    regexp {\s*([^\s=]+)(?:\s*=\s*(.*?)\s*)?} $sub _ key2 val2
                    lappend params [string tolower $key2] [dequote $val2]
                }
                lappend val [string tolower [string trim [lindex $subs 0]]]
                lappend val $params
            }
        } content-disposition - content-type {
            # Value has format "elem;subkey1=subval1;subkey2=subval2".
            set elems [delist semicolon $raw]
            set val [list "" [string tolower [lindex $elems 0]]]
            foreach elem [lrange $elems 1 end] {
                regexp {\s*([^\s=]+)(?:\s*=\s*(.*?)\s*)?} $elem _ key2 val2
                lappend val [string tolower $key2] [dequote $val2]
            }
        } user-agent {
            # Value is a user-agent definition.
            foreach elem [delist space $raw] {
                if {[string index $elem 0] eq "("} {
                    lappend val ([dequote $elem])
                } else {
                    lappend val [dequote $elem]
                }
            }
        } default {
            # Value has format "elem".
            set val $raw
        }}
        dict set header $key $val
    }
    return $header
}

# =============================== wibble core =================================

# Advance to the next zone handler using the specified state list.
proc wibble::nexthandler {args} {
    return -code 5 $args
}

# Send a response to the client.
proc wibble::sendresponse {response} {
    return -code 6 $response
}

# Register a zone handler.
proc wibble::handle {prefix command args} {
    variable zonehandlers
    lappend zonehandlers $prefix $command $args
}

# Get an HTTP request from a client.
proc wibble::getrequest {port chan peerhost peerport} {
    # The HTTP header uses CR/LF line breaks.
    chan configure $chan -translation crlf

    # Receive and parse the first line.
    regexp {^\s*(\S*)\s+(\S*)\s+(.*?)\s*$} [getline $chan] _ method uri protocol
    regexp {^([^?]*)(\?.*)?$} $uri _ path query
    set path [regsub -all {(?:/|^)\.(?=/|$)} [dehex $path] /]
    while {[regexp -indices {(?:/[^/]*/+|^[^/]*/+|^)\.\.(?=/|$)} $path range]} {
        set path [string replace $path {*}$range ""]
    }
    set path [regsub -all {//+} /$path /]

    # Start building the request structure.
    set request [dict create socket $chan peerhost $peerhost peerport $peerport\
        port $port rawtime [clock seconds] time [clock format [clock seconds]]\
        method $method uri $uri path $path protocol $protocol rawheader {}]

    # Parse the query string.
    if {$query ne ""} {
        dict set request rawquery $query
        dict set request query [dequery [string range $query 1 end]]
    }

    # Receive and parse the headers.
    while {[set line [getline $chan]] ne ""} {
        dict lappend request rawheader $line
    }
    dict set request header [deheader [join [dict get $request rawheader] \n]]

    # Process qvalues in accept* headers.
    foreach {header key} {accept type accept-charset charset
    accept-encoding encoding accept-language language} {
        set preferences {}
        if {[dict exists $request header $header]} {
            set options {}
            foreach {option params} [dict get $request header $header] {
                if {[dict exists $params q]
                 && [string is double -strict [dict get $params q]]} {
                    lappend options [list $option [dict get $params q]]
                } else {
                    lappend options [list $option 1]
                }
            }
            foreach elem [lsort -index 1 -decreasing -real $options] {
                lappend preferences [lindex $elem 0]
            }
        }
        dict set request accept $key $preferences
    }

    # Get and parse the request body, if there is one.
    if {$method eq "POST"} {
        # Get the request body.
        if {[dict exists $request header transfer-encoding]
         && [dict get $request header transfer-encoding] eq "chunked"} {
            # Receive chunked request body.
            set data ""
            while {[scan [getline $chan] %x length] == 1 && $length > 0} {
                chan configure $chan -translation binary
                append data [getblock $chan $length]
                chan configure $chan -translation crlf
            }
        } else {
            # Receive non-chunked request body.
            chan configure $chan -translation binary
            set data [getblock $chan [dict get $request header content-length]]
            chan configure $chan -translation crlf
        }

        # Parse the request body.
        dict set request rawpost $data
        set post ""
        if {[dict exists $request header content-type boundary] &&
        [dict get $request header content-type ""] eq "multipart/form-data"} {
            # Interpret multipart/form-data POSTs (required for file uploads).
            set data \r\n$data
            set sep \r\n--[dict get $request header content-type boundary]
            set beg [expr {[string first $sep $data] + 2}]
            set end [expr {[string first $sep $data $beg] - 1}]
            while {$beg < $end} {
                set beg [expr {[string first \n $data $beg] + 1}]
                set part [string range $data $beg $end]
                set split [string first \r\n\r\n $part]
                set val [deheader [string map {\r ""}\
                    [string range $part 0 [expr {$split - 1}]]]]
                dict set val "" [string range $part [expr {$split + 4}] end]
                if {[dict exists $val content-disposition name]} {
                    lappend post [dict get $val content-disposition name] $val
                } else {
                    lappend post "" $val
                }
                set beg [expr {$end + 3}]
                set end [expr {[string first $sep $data $beg] - 1}]
            }
        } elseif {[dict exists $request header content-type]
               && [dict get $request header content-type ""] eq "text/plain"} {
            # Interpret text/plain POSTs.
            foreach elem [lrange [split $data \n] 0 end-1] {
                regexp {([^\r=]*)(?:(=[^\r]*))?} $elem _ key val
                if {$val ne ""} {
                    set val [list "" [string range $val 1 end]]
                }
                lappend post $key $val
            }
        } else {
            # Interpret URL-encoded POSTs (the default).
            set post [dequery $data]
        }
        dict set request post $post
    }

    return $request
}

# Get a response from the zone handlers.
proc wibble::getresponse {request} {
    variable zonehandlers
    set system [list [dict create options {} request $request]]

    # Process all zone handlers.
    foreach {prefix command options} $zonehandlers {
        set match $prefix
        if {[string index $match end] ne "/"} {
            append match /
        }

        # Run the zone handler on all states with requests in the zone.
        set i 0
        foreach {state} $system {
            set path [dict get $state request path]
            if {$path eq $prefix
             || [string equal -length [string length $match] $match $path]} {
                set suffix [string range $path [string length $prefix] end]

                # Replace the options in the state dict.
                dict set state options $options
                dict set state options prefix $prefix
                dict set state options suffix $suffix
                if {[dict exists $options root]} {
                    dict set state options fspath\
                        [dict get $options root]/$suffix
                }

                # Invoke the handler and process its outcome.
                try {
                    {*}$command $state
                } on 5 outcome {
                    # [nexthandler]: Update the system and continue processing.
                    set system [lreplace $system $i $i {*}$outcome]
                } on 6 outcome {
                    # [sendresponse]: A response has been obtained.  Return it.
                    return $outcome
                }
            }
            incr i
        }
    }

    # Return 501 as default response.
    dict create status 501 header {content-type text/plain}\
        content "not implemented: [dict get $request uri]\n"
}

# Main connection processing loop.
proc wibble::process {port socket peerhost peerport} {
    try {
        chan configure $socket -blocking 0
        while {1} {
            # Get request from client, then formulate a response to the reqeust.
            set request [getrequest $port $socket $peerhost $peerport]
            set response [getresponse $request]

            # Get the content size.
            if {[dict exists $response contentfile]} {
                set size [file size [dict get $response contentfile]]
                if {[dict get $request method] ne "HEAD"} {
                    # Open the channel now to catch errors early.
                    set file [open [dict get $response contentfile]]
                    chan configure $file -translation binary
                }
            } elseif {[dict exists $response content]} {
                dict set response content [encoding convertto iso8859-1\
                    [dict get $response content]]
                set size [string length [dict get $response content]]
            } else {
                set size 0
            }

            # Try to parse the Range request header if present.
            set begin 0
            set end [expr {$size - 1}]
            if {[dict exists $request header range]
             && [regexp {^bytes=(\d*)-(\d*)$} [dict get $request header range]\
                    _ begin end]
             && [dict get $response status] == 200} {
                dict set response status 206
                if {$begin eq "" || $begin >= $size} {
                    set begin 0
                }
                if {$end eq "" || $end >= $size || $end < $begin} {
                    set end [expr {$size - 1}]
                }
            }

            # Add content-length and content-range response headers.
            dict set response header content-length [expr {$end - $begin + 1}]
            if {[dict get $response status] == 206} {
                dict set response header content-range "bytes $begin-$end/$size"
            }

            # Send the response header to the client.
            chan puts $socket "HTTP/1.1 [dict get $response status]"
            foreach {key val} [dict get $response header] {
                set normalizedkey [lsearch -exact -sorted -inline -nocase {
                    Accept-Ranges Age Allow Cache-Control Connection
                    Content-Disposition Content-Encoding Content-Language
                    Content-Length Content-Location Content-MD5 Content-Range
                    Content-Type Date ETag Expires Last-Modified Location Pragma
                    Proxy-Authenticate Retry-After Server Set-Cookie Trailer
                    Transfer-Encoding Upgrade Vary Via Warning WWW-Authenticate
                } $key]
                if {$normalizedkey ne ""} {
                    set key $normalizedkey
                }
                foreach line [split $val \n] {
                    chan puts $socket "$key: $line"
                }
            }
            chan puts $socket ""

            # If requested, send the response content to the client.
            if {[dict get $request method] ne "HEAD"} {
                chan configure $socket -translation binary
                if {[dict exists $response contentfile]} {
                    # Asynchronously send response content from a file.
                    chan seek $file $begin
                    chan copy $file $socket -size [expr {$end - $begin + 1}]\
                        -command [resume copydone]
                    if {[llength [set result [suspend copydone]]] == 3} {
                        error [lindex $result 2]
                    }
                    chan close $file
                } elseif {[dict exists $response content]} {
                    # Send buffered response content.
                    chan puts -nonewline $socket [string range\
                        [dict get $response content] $begin $end]
                }
            }

            # Flush the outgoing buffer.
            chan flush $socket
            unset request
        }
    } on error {"" options} {
        # Log errors and report them to the client, if possible.
        variable errorcount
        incr errorcount
        set message "*** INTERNAL SERVER ERROR (BEGIN #$errorcount) ***"
        if {[info exists request]} {
            foreach {key val} [dumprequest $request] {
                append message "\n$key: $val"
            }
        } else {
            append message "\nsocket: $socket"
            append message "\npeerhost: $peerhost"
            append message "\npeerport: $peerport"
            append message "\nrawtime: [clock seconds]"
            append message "\ntime: [clock format [clock seconds]]"
        }
        append message "\nerrorinfo: [dict get $options -errorinfo]"
        append message "\n*** INTERNAL SERVER ERROR (END #$errorcount) ***"
        log $message
        catch {
            set message [encoding convertto iso8859-1 $message]
            chan configure $socket -translation crlf
            chan puts $socket "HTTP/1.1 500 Internal Server Error"
            chan puts $socket "Content-Type: text/plain"
            chan puts $socket "Content-Length: [string length $message]"
            chan puts $socket "Connection: close"
            chan puts $socket ""
            chan configure $socket -translation binary
            chan puts $socket $message
        }
    } finally {
        catch {chan close $socket}
    }
}

# Listen for incoming connections.
proc wibble::listen {port} {
    socket -server [list apply [list {port socket peerhost peerport} {
        coroutine $socket {*}[namespace code process]\
            $port $socket $peerhost $peerport
    } [namespace current]] $port] $port
}

# Log an error.  Feel free to replace this procedure as needed.
proc wibble::log {message} {
    chan puts stderr $message
}

# =============================== example code ================================

# Demonstrate Wibble if being run directly.
if {$argv0 eq [info script]} {
    # Guess the root directory.
    set root [file normalize [file dirname [info script]]]

    # Define zone handlers.
    wibble::handle /vars vars
    wibble::handle / dirslash root $root
    wibble::handle / indexfile root $root indexfile index.html
    wibble::handle / static root $root
    wibble::handle / template root $root
    wibble::handle / script root $root
    wibble::handle / dirlist root $root
    wibble::handle / notfound

    # Start a server and enter the event loop.
    catch {
        wibble::listen 8080
        vwait forever
    }
}

# vim: set sts=4 sw=4 tw=80 et ft=tcl:
