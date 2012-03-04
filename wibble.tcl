#!/usr/bin/env tclsh
# Wibble - a pure-Tcl Web server.  http://wiki.tcl.tk/23626
# Copyright 2011 Andy Goth.  mailto/andrew.m.goth/at/gmail/dot/com
# Available under the Tcl/Tk license.  http://tcl.tk/software/tcltk/license.html

package require Tcl 8.6
package provide wibble 0.2

# Define the wibble namespace.
namespace eval ::wibble {
    variable zonehandlers
}

# ============================== zone handlers ================================

# Define the wibble::zone namespace.
namespace eval ::wibble::zone {
    namespace path ::wibble
}

# Echo request dictionary.
proc ::wibble::zone::vars {state} {
    dict set state response status 200
    dict set state response header content-type "" text/html
    dict set state response content [template {
<html><head><style type="text/css">
    body {font-family: monospace}
    table {border-collapse: collapse; outline: 1px solid #000; width: 100%}
    th {white-space: nowrap; text-align: left; vertical-align: top}
    th, td {border: 1px solid #727772}
    tr:nth-child(odd) {background-color: #ded}
    tr:nth-child(even) {background-color: #eee}
    th.title {background-color: #8d958d; text-align: center}
</style></head><body><table>
% dict for {dictname dictval} $state {
    <tr><th class="title" colspan="2">[enhtml $dictname]</th></tr>
%   if {$dictname in {request response}} {
%       set dictval [dumpstate $dictval]
%   }
%   dict for {key val} $dictval {
    <tr><th>[enhtml $key]</th><td>[enhtml $val]</td></tr>
%   }
% }
</table></body></html>}]
    sendresponse [dict get $state response]
}

# Redirect when a directory is requested without a trailing slash.
proc ::wibble::zone::dirslash {state} {
    dict with state request {}; dict with state options {}
    if {[file isdirectory $fspath] && [string index $suffix end] ni {/ ""}} {
        append path /
        if {[info exists rawquery]} {
            append path $rawquery
        }
        redirect $path
    }
}

# Rewrite directory requests to search for an indexfile.
proc ::wibble::zone::indexfile {state} {
    dict with state request {}; dict with state options {}
    if {[file isdirectory $fspath]} {
        if {[string index $path end] ne "/"} {
            append path /
        }
        set newstate $state
        dict set newstate request path $path$indexfile
        nexthandler $newstate $state
    }
}

# Generate directory listings.
proc ::wibble::zone::dirlist {state} {
    dict with state request {}; dict with state options {}
    if {![file isdirectory $fspath]} {
        # Pass if the requested object is not a directory or doesn't exist.
    } elseif {[file readable $fspath]} {
        # If the directory is readable, generate a listing.
        dict set state response status 200
        dict set state response header content-type "" text/html
        dict set state response content [template {
<html><body>
% if {$path ne "/"} {
    <li><a href="../">../</a></li>
% }
% foreach elem [lsort [glob -nocomplain -tails -types d -directory $fspath *]] {
    <li><a href="[enhex $elem/]">[enhtml $elem/]</a></li>
% }
% foreach elem [lsort [glob -nocomplain -tails -types f -directory $fspath *]] {
    <li><a href="[enhex $elem]">[enhtml $elem]</a></li>
% }
</body></html>}]
        sendresponse [dict get $state response]
    } else {
        # But if it isn't readable, generate a 403.
        forbidden $state
    }
}

# Execute scripts.
proc ::wibble::zone::scriptfile {state} {
    dict with state request {}; dict with state options {}
    if {[file readable $fspath.script]} {
        dict set state response status 200
        source $fspath.script
        sendresponse [dict get $state response]
    }
}

# Execute templates.
proc ::wibble::zone::templatefile {state} {
    dict with state request {}; dict with state options {}
    if {[file readable $fspath.tmpl]} {
        set chan [open $fspath.tmpl]
        set body [chan read $chan]
        chan close $chan
        dict set state response status 200
        dict set state response content [template $body]
        sendresponse [dict get $state response]
    }
}

# Guess the content type from the URI extension.
proc ::wibble::zone::contenttype {state} {
    dict with state request {}; dict with state options {}
    set extension [string tolower [string range [file extension $path] 1 end]]
    foreach {type pattern} $typetable {
        if {[regexp -nocase -- $pattern $extension]} {
            dict set state response header content-type "" $type
            nexthandler $state
        }
    }
}

# Send static files.
proc ::wibble::zone::staticfile {state} {
    dict with state request {}; dict with state options {}
    if {![file isdirectory $fspath] && [file exists $fspath]} {
        dict set state response status 200
        dict set state response contentfile $fspath
        sendresponse [dict get $state response]
    }
}

# Send a 301 Moved Permanently.
proc ::wibble::zone::redirect {newurl {state ""}} {
    dict set state response status 301
    dict set state response header location $newurl
    sendresponse [dict get $state response]
}

# Send a 403 Forbidden.
proc ::wibble::zone::forbidden {state} {
    dict set state response status 403
    dict set state response header content-type {"" text/plain charset utf-8}
    dict set state response content "forbidden: [dict get $state request uri]\n"
    sendresponse [dict get $state response]
}

# Send a 404 Not Found.
proc ::wibble::zone::notfound {state} {
    dict set state response status 404
    dict set state response header content-type {"" text/plain charset utf-8}
    dict set state response content "not found: [dict get $state request uri]\n"
    sendresponse [dict get $state response]
}

# ============================ utility procedures =============================

# Expand a template.
proc ::wibble::template {body} {
    set script ""
    set pos 0
    foreach match [regexp -line -all -inline -indices {^%.*$} $body] {
        lassign $match from to
        set str [string range $body $pos [expr {$from - 1}]]
        if {$str ne ""} {
            append script "append # \[" [list subst $str] \]\n
        }
        append script [string range $body [expr {$from + 1}] $to]\n
        set pos [expr {$to + 2}]
    }
    set str [string range $body $pos end]
    if {$str ne ""} {
        append script "append # \[" [list subst $str] \]
    }
    uplevel 1 "set # {}; $script; set #"
}

# Flatten a request/response state dictionary into a form that's easier to log.
proc ::wibble::dumpstate {data {prefix ""}} {
    if {![llength $data]} {
        return [list $prefix ""]
    }
    set result {}
    dict for {key val} $data {
        set key [concat $prefix [list $key]]
        if {$key in {header accept query "header content-type"}
         || (([lindex $key 0] in {post query}
           || [lrange $key 0 1] in {"header cookie" "header set-cookie"})
          && ([llength $key] < 3
           || ([llength $key] == 3 && [lindex $key 2] ne "")))} {
            lappend result {*}[dumpstate $val $key]
        } elseif {[string length $val] > 512} {
            lappend result $key (len=[string length $val])
        } else {
            lappend result $key $val
        }
    }
    return $result
}

# ========================= network input procedures ==========================

# Get a line of data from the current coroutine's socket.
proc ::wibble::getline {} {
    set socket [namespace tail [info coroutine]]
    while {1} {
        if {[chan gets $socket line] >= 0} {
            return $line
        } elseif {[chan eof $socket]} {
            return -level [info level]
        } elseif {[chan pending input $socket] > 4096} {
            error "line length exceeds limit of 4096 bytes"
        }
        icc get [info coroutine] readable
    }
}

# Get a block of data from the current coroutine's socket.
proc ::wibble::getblock {size} {
    set socket [namespace tail [info coroutine]]
    while {1} {
        set chunklet [chan read $socket $size]
        set size [expr {$size - [string length $chunklet]}]
        append chunk $chunklet
        if {[chan eof $socket]} {
            return -level [info level]
        } elseif {$size == 0} {
            return $chunk
        }
        icc get [info coroutine] readable
    }
}

# ==================== conversion and parsing procedures ======================

# Encode for HTML by substituting angle brackets, ampersands, line breaks, and
# space sequences.
proc ::wibble::enhtml {str} {
    string map {< &lt; > &gt; & &amp; \r "" \n "<br />\n" "  " \ &\#160;} $str
}

# Encode for HTML tag attribute by substituting angle brackets, ampersands,
# double quotes, and space sequences.
proc ::wibble::enattr {str} {
    string map {< &lt; > &gt; & &amp; \r "" \n "" \" &quot; "  " \ &\#160;} $str
}

# Encode for HTML <pre> by substituting angle brackets and ampersands.
proc ::wibble::enpre {str} {
    string map {< &lt; > &gt; & &amp; \r ""} $str
}

# Encode a query string.  The caller must prepend the question mark.
proc ::wibble::enquery {args} {
    set query {}
    set encode {apply {{str} {string map { " " +}\
        [enhex $str {[^-^,./'|!$\w ]}]} ::wibble}}
    foreach {key val} [concat {*}$args] {
        if {[dict exists $val ""]} {
            lappend query [{*}$encode $key]=[{*}$encode [dict get $val ""]]
        } else {
            lappend query [{*}$encode $key]
        }
    }
    join $query &
}

# Decode a query string into a list.  The caller must strip the question mark.
proc ::wibble::dequery {str} {
    set query {}
    foreach elem [split $str &] {
        regexp {^([^=]*)(?:(=.*))?$} $elem _ key val
        if {$val ne ""} {
            set val [list "" [dehex [string map {+ " "}\
                [string range $val 1 end]]]]
        }
        lappend query [dehex [string map {+ " "} $key]] $val
    }
    return $query
}

# Encode by substituting most non-alphanumerics with hexadecimal codes.
proc ::wibble::enhex {str {pattern {[^-^,./'=+|!$\w]}}} {
    set pos 0
    while {[regexp -indices -start $pos $pattern $str range]} {
        binary scan [string range $str {*}$range] H2 char
        set str [string replace $str {*}$range %$char]
        set pos [expr {[lindex $range 0] + 3}]
    }
    return $str
}

# Decode hexadecimal encoding.
proc ::wibble::dehex {str} {
    set pos 0
    while {[regexp -indices -start $pos {%([[:xdigit:]]{2})} $str range code]} {
        set char [binary format H2 [string range $str {*}$code]]
        set str [string replace $str {*}$range $char]
        set pos [expr {[lindex $range 0] + 1}]
    }
    return $str
}

# Encode an HTTP time/date.
proc ::wibble::entime {time} {
    switch [lindex $time 0] {
        abstime {set time [lindex $time 1]}
        reltime {set time [expr {[clock seconds] + [lindex $time 1]}]}
    }
    clock format $time -format "%a %d-%b-%Y %T %Z" -timezone :GMT
}

# Decode an HTTP time/date.
proc ::wibble::detime {str} {
    list abstime [clock scan $str]
}

# Decode header list encoding.
proc ::wibble::delist {separator str} {
    regexp -all -inline [dict get {
semicolon {(?:[^;"=]+=)?(?:[Ww]/)?"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^;]+}
comma     {(?:[^,"=]+=)?(?:[Ww]/)?"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^,]+}
semicomma {(?:[^;,"=]+=)?"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^;,]+}
space     {"(?:[^\\"]|\\.)*"|\((?:[^\\()]|\\.)*\)|[^"()\\\s]+}
    } $separator] $str
}

# Encode HTTP header quoting when appropriate.
proc ::wibble::enquote {str} {
    if {$str eq "" || [regexp {[\0-\040\177\(\)<>@,;:\\"/\[\]\?={}]} $str]} {
        return \"[regsub -all {[\0-\010\012-\037\177"\\]} $str {\\&}]\"
    } else {
        return $str
    }
}

# Decode HTTP header quoting.
proc ::wibble::dequote {str} {
    if {([string index $str 0] eq "\"" && [string index $str end] eq "\"")
     || ([string index $str 0] eq "(" && [string index $str end] eq ")")} {
        regsub -all {\\(.)} [string range $str 1 end-1] {\1}
    } else {
        return $str
    }
}

# Encode an HTTP entity tag.
proc ::wibble::entag {tag} {
    lassign $tag type val
    switch $type {
    tag {return \"[regsub -all {[\0-\010\012-\037\177"\\]} $val {\\&}]\"}
    weaktag {return W/\"[regsub -all {[\0-\010\012-\037\177"\\]} $val {\\&}]\"}
    }
}

# Decode an HTTP entity tag.
proc ::wibble::detag {str} {
    if {[string range $str 0 2] in {W/\" w/\"}} {
        list weaktag [dequote [string range $str 2 end]]
    } else {
        list tag [dequote $str]
    }
}

# Encode HTTP headers.
proc ::wibble::enheader {header} {
    set str ""
    set nl ""
    dict for {key val} $header {
        if {![llength $val]} {continue}

        set comma ""
        switch $key {
        set-cookie {
            # Value is a list of cookie definitions.
            dict for {key2 val2} $val {
                append str "$nl$key: [enhex $key2]=[enhex [dict get $val2 ""]]"
                dict for {key3 val3} $val2 {
                    switch $key3 {
                    domain - path {
                        append str \;$key3=[string map {; %3b} $val3]
                    } port {
                        append str \;port
                        if {[llength $val3]} {
                            append str =\"[join $val3 ,]\"
                        }
                    } discard - httponly - secure {
                        append str \;$key3
                    } expires {
                        switch [lindex $val3 0] {
                            abstime {append str \;expires=[entime $val3 1]}
                            reltime {append str \;max-age=[lindex $val3 1]}
                        }
                    }}
                }
                set nl \n
            }
        } cache-control - pragma {
            # Value has format "subkey1=subval1,subkey2=subval2,subkey3".
            append str "$nl$key: "
            dict for {key2 val2} $val {
                append str $comma$key2
                if {[dict exists $val2 ""]} {
                    if {$key eq "cache-control"&& $key2 in {private no-cache}} {
                        append str =\"[join [dict get $val2 ""] ,]\"
                    } else {
                        append str =[enquote [dict get $val2 ""]]
                    }
                }
                set comma ,
            }
        } accept-ranges - allow - connection - content-encoding -
        content-language - proxy-authenticate - trailer - upgrade - vary - via -
        www-authenticate {
            # Value has format "elem1,elem2".
            append str "$nl$key: "
            foreach val2 $val {
                append str $comma[enquote $val2]
                set comma ,
            }
        } warning {
            # Value has format "elem1.1 elem1.2 elem1.3,elem2.1 elem2.2".
            append str "$nl$key: "
            foreach val2 $val {
                append str $comma
                set space ""
                foreach val3 $val2 {
                    append str $space[enquote $val3]
                    set space " "
                }
                set comma ,
            }
        } transfer-encoding {
            # Value has format "elem1;subkey1=subval1;subkey2=subval2,elem2".
            append str "$nl$key: "
            foreach val2 $val {
                append str [dict get $val2 ""]
                dict for {key3 val3} $val2 {
                    if {$key3 ne ""} {
                        append str \;$key3=[enquote $val3]
                    }
                }
            }
        } content-disposition - content-type {
            # Value has format "elem;subkey1=subval1;subkey2=subval2".
            append str "$nl$key: [dict get $val ""]"
            dict for {key2 val2} $val {
                if {$key2 ne ""} {
                    append str \;$key2=[enquote $val2]
                }
            }
        } server {
            # Value is a server agent definition.
            append str $nl$key:
            foreach val2 $val {
                if {[string index $elem 0] eq "("} {
                    append str " ([regsub -all {[\0-\010\012-\037\177()\\]}\
                        $val2 {\\&}])"
                } else {
                    append str " [enquote $val2]"
                }
            }
        } date - expires - last-modified {
            # Value is an absolute time.
            append str "$nl$key: [entime $val]"
        } retry-after {
            # Value is an absolute or relative time.
            switch [lindex $val 0] {
                abstime {append str "$nl$key: [entime $val 1]"
                reltime {append str "$nl$key: [lindex $val 1]"}
            }}
        } etag {
            # Value is an entity tag.
            append str "$nl$key: [entag $val]"
        } age - content-length - content-location - content-md5 -
        content-range - location {
            # Value is a never-quoted string.
            append str "$nl$key: $val"
        } default {
            # Value is a sometimes-quoted string.
            append str "$nl$key: [enquote $val]"
        }}
        set nl \n
    }
    return $str
}

# Decode HTTP headers.
proc ::wibble::deheader {str} {
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
            # Value is one or more cookie definitions.
            set common {}
            set cookie ""
            foreach elem [delist semicomma $raw] {
                regexp {\s*([^\s=]*)(?:\s*=(.*))?} $elem _ key2 val2
                set key2 [string tolower $key2]
                if {[string index $key2 0] eq "\$"} {
                    set key2 [string trim [string range $key2 1 end]]
                    if {$cookie eq ""} {
                        dict set common $key2 [dequote $val2]
                    } else {
                        dict set params $key2 [dequote $val2]
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
            # Value has format "subkey1=subval1,subkey2=subval2,subkey3".
            foreach elem [delist comma $raw] {
                regexp {\s*([^\s=]+)(?:\s*(=.*))?} $elem _ key2 val2
                if {$val2 ne ""} {
                    set val2 [dequote [string trim [string range $val2 1 end]]]
                    if {$key eq "cache-control"&& $key2 in {private no-cache}} {
                        set val2 [delist comma $val2]
                    }
                    set val2 [list "" $val2]
                }
                lappend val [string tolower $key2] $val2
            }
        } connection - content-encoding - content-language - if-match -
        if-none-match - trailer - upgrade - vary - via {
            # Value has format "elem1,elem2".
            foreach elem [delist comma $raw] {
                if {$key in {if-match if-none-match}} {
                    lappend val [detag [string trim $elem]]
                } else {
                    lappend val [dequote [string trim $elem]]
                }
            }
        } warning {
            # Value has format "elem1.1 elem1.2 elem1.3,elem2.1 elem2.2".
            foreach elem [delist comma $raw] {
                set val2 {}
                foreach elem2 [delist space $elem] {
                    lappend val2 [dequote $elem2]
                }
                lappend val $val2
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
        } date - expires - if-modified-since - if-unmodified-since -
        last-modified {
            # Value is an absolute time.
            set val [detime $raw]
        } if-range {
            # Value is an absolute time or an entity tag.
            if {[string index $raw end] eq "\""} {
                set val [detag $raw]
            } else {
                set val [detime $raw]
            }
        } default {
            # Value has format "elem".
            set val [dequote $raw]
        }}
        dict set header $key $val
    }
    return $header
}

# =================== inter-coroutine communication system ====================

# The inter-coroutine communication procedures are in the [icc] ensemble.
namespace eval ::wibble::icc {
    namespace export configure get put
    namespace ensemble create
    variable feeds
}

# Lapse (remove) a feed that nothing's interested in anymore.
proc ::wibble::icc::lapse {fid} {
    variable feeds

    # Clean up the feed's data structures.
    set lapsescript [dict get $feeds $fid lapsescript]
    dict unset feeds $fid

    # Run the lapse script, which may be empty string.
    uplevel #0 $lapsescript
}

# Adjust an ICC feed's configuration, creating the feed in the process.
# [icc configure $fid accept|reject ?filter? ?...?]
# [icc configure $fid lapse ?timeout_milliseconds? ?lapsescript?]
proc ::wibble::icc::configure {fid operation args} {
    variable feeds

    # Initialize the feed if it doesn't already exist.
    if {![info exists feeds] || ![dict exists $feeds $fid]} {
        dict set feeds $fid {acceptable "" lapsetime "" lapsescript ""
            lapsecancel "" suspended "" pending ""}
    }

    # Reset the feed's lapse timeout.
    if {[dict get $feeds $fid lapsecancel] ne ""} {
        after cancel [dict get $feeds $fid lapsecancel]
        dict set feeds $fid lapsecancel ""
    }

    # Process the requested operation.
    switch $operation {
    lapse {
        # Store arguments into feed structure, defaulting to "".
        dict set feeds $fid lapsetime [lindex $args 0]
        dict set feeds $fid lapsescript [lindex $args 1]
    } accept {
        # Append the arguments to the list of accepted filters.
        dict set feeds $fid acceptable [lsort -unique [concat\
            [dict get $feeds $fid acceptable] $args]]
    } reject {
        # Remove all filters that match any of the argument patterns.
        set index 0
        foreach filter [dict get $feeds $fid acceptable] {
            foreach pattern $args {
                if {[string match $pattern $filter]} {
                    dict set feeds $fid acceptable [lreplace\
                        [dict get $feeds $fid acceptable] $index $index]
                    incr index -1
                    break
                }
            }
            incr index
        }
    }}

    # Restart the feed's lapse timeout.
    if {[dict get $feeds $fid lapsetime] ne ""} {
        dict set feeds $fid lapsecancel [after [dict get $feeds $fid lapsetime]\
            [list ::wibble::icc::lapse $fid]]
    }
}

# Get a list of events on one or more feeds matching any of the filters.
proc ::wibble::icc::get {fids filters {timeout ""}} {
    variable feeds

    # Reset the feed lapse timeouts, and check for pending events.
    set result {}
    set index 0
    foreach fid $fids {
        # Reset the feed's lapse timeout.
        if {[dict get $feeds $fid lapsecancel] ne ""} {
            after cancel [dict get $feeds $fid lapsecancel]
            dict set feeds $fid lapsecancel ""
        }

        # Gather the pending events that match the request filters.
        foreach entry [dict get $feeds $fid pending] {
            foreach filter $filters {
                if {[string match $filter [lindex $entry 0]]} {
                    dict set feeds $fid pending [lreplace\
                        [dict get $feeds $fid pending] $index $index]
                    lappend result $entry
                    incr index -1
                    break
                }
            }
            incr index
        }
    }

    # If no acceptable events were pending, wait for one to occur.
    if {![llength $result]} {
        # Install wake-up handlers for readability and timeout, as requested.
        set coro [info coroutine]
        set socket [namespace tail $coro]
        if {[set readable [expr {"readable" in $filters && $coro in $fids}]]} {
            chan event $socket readable [list $coro readable]
        }
        if {$timeout ne ""} {
            lappend filters timeout
            if {$coro ni $fids} {
                lappend fids $coro
            }
            set timeoutcancel [after $timeout [list $coro timeout]]
        }

        # Wait for an event.  Maintain each feed's list of suspended coroutines.
        foreach fid $fids {
            dict set feeds $fid suspended $coro $filters
        }
        set result [list [yield]]
        foreach fid $fids {
            dict unset feeds $fid suspended $coro
        }

        # Remove the readability and timeout handlers.
        if {$timeout ne "" && [lindex $result 0 0] ne "timeout"} {
            after cancel $timeoutcancel
        }
        if {$readable} {
            chan event $socket readable ""
        }
    }

    # Restart the lapse timeouts for the feeds monitored by this coroutine.
    foreach fid $fids {
        if {[dict get $feeds $fid lapsetime] eq ""} {continue}

        if {[dict get $feeds $fid lapsecancel] ne ""} {
            after cancel [dict get $feeds $fid lapsecancel]
        }
        dict set feeds $fid lapsecancel [after [dict get $feeds $fid lapsetime]\
            [list ::wibble::icc::lapse $fid]]
    }

    # Return the event data.
    return $result
}

# Send event data to the named feeds, or all if "*".
proc ::wibble::icc::put {fids event args} {
    variable feeds

    # Expand "*" to a list of all feeds that exist at the time [put] is called.
    if {$fids eq "*"} {
        set fids [dict keys $feeds]
    }

    # Insist on running from the event loop, never from within a coroutine.
    if {[info coroutine] ne ""} {
        after 0 [concat [list ::wibble::icc::put $fids $event] $args]
        return
    }

    # Send the event to all feeds whose filters accept it.
    set argument [concat [list $event] $args]
    foreach fid $fids {
        if {[dict exists $feeds $fid]} {
            foreach filter [dict get $feeds $fid acceptable] {
                if {[string match $filter $event]} {
                    # Send event to a suspended coroutine watching the feed.
                    set found 0
                    dict for {coro filters} [dict get $feeds $fid suspended] {
                        foreach filter $filters {
                            if {[string match $filter $event]} {
                                if {[info commands $coro] ne ""} {
                                    $coro $argument
                                }
                                set found 1
                                break
                            }
                        }
                    }

                    # If no suspended coroutine, enqueue the event.
                    if {!$found} {
                        dict set feeds $fid pending [concat\
                            [dict get $feeds $fid pending] [list $argument]]
                    }
                    break
                }
            }
        }
    }
}

# =============================== wibble core =================================

# Advance to the next zone handler using the specified state list.
proc ::wibble::nexthandler {args} {
    return -code 5 $args
}

# Send a response to the client.
proc ::wibble::sendresponse {response} {
    return -code 6 $response
}

# Register a zone handler.
proc ::wibble::handle {prefix cmd args} {
    variable zonehandlers
    set name [namespace eval zone [list namespace which [lindex $cmd 0]]]
    if {$name eq ""} {
        error "invalid command name \"$cmd\""
    }
    lappend zonehandlers $prefix [concat [list $name] [lrange $cmd 1 end]] $args
}

# Add, modify, or cancel coroutine cleanup scripts.
proc ::wibble::cleanup {key script} {
    upvar #1 cleanup cleanup
    if {$script ne ""} {
        dict set cleanup $key $script
    } else {
        dict unset cleanup $key
    }
}

# Get an HTTP request from a client.
proc ::wibble::getrequest {port chan peerhost peerport} {
    # The HTTP header uses CR/LF line breaks.
    chan configure $chan -translation crlf

    # Receive and parse the first line.  Normalize the path.
    regexp {^\s*(\S*)\s+(\S*)\s+(\S*)} [getline] _ method uri protocol
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
    while {[set line [getline]] ne ""} {
        dict lappend request rawheader $line
    }
    dict set request header [deheader [join [dict get $request rawheader] \n]]

    # Process qvalues in accept* headers.
    foreach {header key} {accept type   accept-charset charset
    accept-encoding encoding   accept-language language   te transfercoding} {
        set preferences {}
        if {[dict exists $request header $header]} {
            set options {}
            dict for {option params} [dict get $request header $header] {
                if {![dict exists $params q]
                 || ![string is double -strict [dict get $params q]]} {
                    lappend options [list $option 1]
                } elseif {[dict get $params q] > 0} {
                    lappend options [list $option [dict get $params q]]
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
            while {[scan [getline] %x length] == 1 && $length > 0} {
                chan configure $chan -translation binary
                append data [getblock $length]
                chan configure $chan -translation crlf
            }
        } else {
            # Receive non-chunked request body.
            chan configure $chan -translation binary
            set data [getblock [dict get $request header content-length]]
            chan configure $chan -translation crlf
        }

        # Parse the request body.
        dict set request rawpost $data
        set post ""
        switch [if {[dict exists $request header content-type ""]} {
            dict get $request header content-type ""
        }] {
        multipart/form-data {
            # Interpret multipart/form-data (required for file uploads).
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
        } text/plain {
            # Interpret text/plain POSTs.
            foreach elem [lrange [split $data \n] 0 end-1] {
                regexp {([^\r=]*)(?:(=[^\r]*))?} $elem _ key val
                if {$val ne ""} {
                    set val [list "" [string range $val 1 end]]
                }
                lappend post $key $val
            }
        } text/xml {
            # Interpret text/xml POSTs.
            dict set post xml "" [dehex $data]
        } application/x-www-form-urlencoded - default {
            # Interpret URL-encoded POSTs (the default).
            set post [dequery $data]
        }}
        dict set request post $post
    }

    # The request has been received and parsed.  Return it to the caller.
    return $request
}

# Get a response from the zone handlers.
proc ::wibble::getresponse {request} {
    variable zonehandlers
    set system [list [dict create options {} request $request response {}]]

    # Process all zone handlers.
    foreach {prefix command options} $zonehandlers {
        set match $prefix
        if {[string index $match end] ne "/"} {
            append match /
        }

        # Run the zone handler on all states with request paths inside the zone.
        set i 0
        foreach state $system {
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
    dict create status 501 header {content-type {"" text/plain charset utf-8}}\
        content "not implemented: [dict get $request uri]\n"
}

# Default send handler: send the response to the client using HTTP.
proc ::wibble::defaultsend {socket request response} {
    # Get the content channel and/or size.
    set size 0
    if {[dict exists $response contentfile]} {
        set size [file size [dict get $response contentfile]]
        if {[dict get $request method] ne "HEAD"} {
            set file [open [dict get $response contentfile]]
            cleanup close_content_file [list chan close $file]
        }
    } elseif {[dict exists $response contentchan]} {
        if {[dict exists $response contentsize]} {
            set size [dict get $response contentsize]
        }
        set file [dict get $response contentchan]
        cleanup close_content_file [list chan close $file]
    } elseif {[dict exists $response content]} {
        dict set response content [encoding convertto iso8859-1\
            [dict get $response content]]
        set size [string length [dict get $response content]]
    }

    # Parse range request header, and add content-range/-length headers.
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
        dict set response header content-range "bytes $begin-$end/$size"
    }
    dict set response header content-length [expr {$end - $begin + 1}]

    # Send the response header to the client.
    chan puts $socket "HTTP/1.1 [dict get $response status]"
    chan puts $socket [enheader [dict get $response header]]\n

    # If requested, send the response content to the client.
    if {[dict get $request method] ne "HEAD"} {
        chan configure $socket -translation binary
        if {[info exists file]} {
            # Asynchronously send response content from a channel.
            set coro [info coroutine]
            chan configure $file -translation binary
            chan seek $file $begin
            chan copy $file $socket -size [expr {$end - $begin + 1}]\
                -command [list ::wibble::icc put $coro copydone]
            if {[llength [set data [icc get $coro copydone]]] == 3} {
                error [lindex $data 2]
            }
        } elseif {[dict exists $response content]} {
            # Send buffered response content.
            chan puts -nonewline $socket [string range\
                [dict get $response content] $begin $end]
        }
    }

    # Close the content file or channel.
    if {[info exists file]} {
        chan close $file
        cleanup close_content_file ""
    }

    # Return 1 to keep going or 0 if the connection needs to close.
    expr {![dict exists $request header connection]
       || ![string equal -nocase [dict get $request header connection] close]}
}

# Main connection processing loop.
proc ::wibble::process {port socket peerhost peerport} {
    try {
        # Perform initial configuration.
        set coro [info coroutine]
        cleanup close_client_socket [list chan close $socket]
        cleanup unset_feed [list dict unset ::wibble::icc::feeds $coro]
        icc configure $coro accept readable copydone timeout
        chan configure $socket -blocking 0

        # Main loop.
        while {1} {
            # Get request from client, then formulate a response to the reqeust.
            set request [getrequest $port $socket $peerhost $peerport]
            set response [getresponse $request]

            # Determine which command should be used to send the response.
            if {[dict exists $response sendcommand]} {
                set sendcommand [dict get $response sendcommand]
            } else {
                set sendcommand ::wibble::defaultsend
            }

            # Invoke the send command, and terminate or continue as requested.
            if {[{*}$sendcommand $socket $request $response]} {
                catch {chan flush $socket}
                unset request response
            } else {
                chan close $socket
                break
            }
        }
    } on error {"" options} {
        # Pass errors to the panic handler.
        foreach var {request response} {
            if {![info exists $var]} {
                set $var {}
            }
        }
        panic $options $port $socket $peerhost $peerport $request $response
    } finally {
        # Always run scheduled cleanup scripts on coroutine termination.
        foreach script [lreverse [dict values $cleanup]] {
            catch $script
        }
    }
}

# Listen for incoming connections.
proc ::wibble::listen {port} {
    socket -server [list apply {{port socket peerhost peerport} {
        coroutine $socket ::wibble::process $port $socket $peerhost $peerport
    } ::wibble} $port] $port
}

# ========================= customizable procedures ===========================

# Log a message.  Feel free to replace this procedure as needed.
proc ::wibble::log {message} {
    chan puts stderr $message
}

# Log errors and report them to the client, if possible.  Customize as needed.
proc ::wibble::panic {options port socket peerhost peerport request response} {
    variable errorcount
    incr errorcount
    set message "*** INTERNAL SERVER ERROR (BEGIN #$errorcount) ***"
    if {[dict size $request]} {
        dict for {key val} [dumpstate $request] {
            append message "\n$key: $val"
        }
    } else {
        append message "\nport: $port"
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
        chan configure $socket -translation crlf
        chan puts $socket "HTTP/1.1 500 Internal Server Error"
        chan puts $socket "Content-Type: text/plain;charset=utf-8"
        chan puts $socket "Content-Length: [string length $message]"
        chan puts $socket "Connection: close"
        chan puts $socket ""
        chan configure $socket -translation lf -encoding utf-8
        chan puts $socket $message
    }
}

# =============================== example code ================================

# Demonstrate Wibble if being run directly.
if {$argv0 eq [info script]} {
    # Guess the root directory.
    set root [file normalize [file dirname [info script]]]

    # Define zone handlers.
    set ::wibble::zonehandlers {}
    ::wibble::handle /vars vars
    ::wibble::handle / dirslash root $root
    ::wibble::handle / indexfile root $root indexfile index.html
    ::wibble::handle / contenttype typetable {
application/javascript  ^js$                  application/json  ^json$
application/pdf ^pdf$                         audio/mid      ^(?:midi?|rmi)$
audio/mp4       ^m4a$                         audio/mpeg     ^mp3$
audio/ogg       ^(?:flac|og[ag]|spx)$         audio/vnd.wave ^wav$
audio/webm      ^webm$                        image/bmp      ^bmp$
image/gif       ^gif$                         image/jpeg     ^(?:jp[eg]|jpeg)$
image/png       ^png$                         image/svg+xml  ^svg$
image/tiff      ^tiff?$                       text/css       ^css$
text/html       ^html?$                       text/plain     ^txt$
text/xml        ^xml$                         video/mp4      ^(?:mp4|m4[bprv])$
video/mpeg      ^(?:m[lp]v|mp[eg]|mpeg|vob)$  video/ogg      ^og[vx]$
video/quicktime ^(?:mov|qt)$                  video/x-ms-wmv ^wmv$
    }
    ::wibble::handle / staticfile root $root
    ::wibble::handle / scriptfile root $root
    ::wibble::handle / templatefile root $root
    ::wibble::handle / dirlist root $root
    ::wibble::handle / notfound

    # Start a server, enter the event loop, and provide a console if needed.
    if {[catch {::wibble::listen 8080}]} {
        # If Wibble is already loaded, do nothing.
    } elseif {[catch {package present Tk}]} {
        # If there's no Tk, provide no interface, and only enter the event loop.
        vwait forever
    } elseif {![catch {console show}]} {
        # Use the built-in Tk console if it's there, but customize it a bit.
        wm withdraw .
        console eval [list proc ReloadWibble {} [list consoleinterp eval [list\
            source [info script]]]]
        console eval {
            wm title . "Wibble Web Server"
            wm protocol . WM_DELETE_WINDOW exit
            .menubar.file delete 1
            .menubar.file insert 1 command -label "Reload Wibble" -underline 0\
                -accelerator Ctrl+R -command ReloadWibble
            .menubar.file entryconfigure 2 -accelerator Ctrl+L
            bind . <Control-r> ReloadWibble
        }
    } else {
        # Or provide a command entry window and use normal logging.
        wm title . "Wibble Web Server"
        wm protocol . WM_DELETE_WINDOW exit
        pack [text .e -height 2] -fill both -expand 1
        bind .e <Return> {
            if {!(%s & 4) && [info complete [set command [.e get 0.1 end]]]} {
                if {![catch $command result]} {
                    ::wibble::log "%% $command<OK>$result"
                    .e delete 0.1 end
                } else {
                    ::wibble::log "%% $command<ERROR>$result"
                }
                break
            }
        }
    }
}

# vim: set sts=4 sw=4 tw=80 et ft=tcl:
