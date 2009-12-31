#!/bin/sh
#
# Wibble - a pure-Tcl Web server.  http://wiki.tcl.tk/23626
# Copyright 2009 Andy Goth.  mailto:unununium/at/aircanopy/dot/net
# Available under the Tcl/Tk license.  http://tcl.tk/software/tcltk/license.html
#
# The next line restarts with tclsh.\
exec tclsh "$0" ${1+"$@"}

package require Tcl 8.6
package provide wibble 0.1

# Define the wibble namespace.
namespace eval wibble {
    variable zones {}
}

# Echo request dictionary.
proc wibble::vars {request response} {
    dict set response status 200
    dict set response header content-type text/html
    dict set response content {<html><body><table border="1">}
    dict for {key val} $request {
        if {$key in {header query}} {
            set newval ""
            dict for {subkey subval} $val {
                append newval "<b>[list $subkey]</b> [list $subval] "
            }
            set val $newval
        }
        dict append response content <tr><td><b>$key</b></td><td>$val</td></tr>
    }
    dict append response content </table></body></html>\n
    sendresponse $response
}

# Redirect when a directory is requested without a trailing slash.
proc wibble::dirslash {request response} {
    dict with request {
        if {[file isdirectory $fspath]
         && [string index $suffix end] ni {/ ""}} {
            dict set response status 301
            dict set response header location $path/$rawquery
            sendresponse $response
        } else {
            nexthandler $request $response
        }
    }
}

# Rewrite directory requests to search for an indexfile.
proc wibble::indexfile {request response} {
    dict with request {
        if {[file isdirectory $fspath]} {
            if {[string index $path end] ne "/"} {
                append path /
            }
            set newrequest $request
            dict set newrequest path $path$indexfile
            nexthandler $newrequest $response $request $response
        } else {
            nexthandler $request $response
        }
    }
}

# Generate directory listings.
proc wibble::dirlist {request response} {
    dict with request {
        if {![file isdirectory $fspath]} {
            # Pass if the requested object is not a directory or doesn't exist.
            nexthandler $request $response
        } elseif {[file readable $fspath]} {
            # If the directory is readable, generate a listing.
            dict set response status 200
            dict set response header content-type text/html
            dict set response content <html><body>
            foreach elem [concat [list ..]\
                    [lsort [glob -nocomplain -tails -directory $fspath *]]] {
                dict append response content "<a href=\"$elem\">$elem</a><br />"
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
}

# Process templates.
proc wibble::template {request response} {
    dict with request {
        if {[file readable $fspath.tmpl]} {
            dict set response status 200
            dict set response header content-type text/plain
            dict set response content ""
            set chan [open $fspath.tmpl]
            applytemplate "dict append response content" [read $chan]
            chan close $chan
            sendresponse $response
        } else {
            nexthandler $request $response
        }
    }
}

# Send static files.
proc wibble::static {request response} {
    dict with request {
        if {![file isdirectory $fspath] && [file exists $fspath]} {
            dict set response status 200
            dict set response contentfile $fspath
            sendresponse $response
        } else {
            nexthandler $request $response
        }
    }
}

# Send a 404.
proc wibble::notfound {request response} {
    dict set response status 404
    dict set response header content-type text/plain
    dict set response content "can't find [dict get $request uri]\n"
    sendresponse $response
}

# Apply a template.
proc wibble::applytemplate {command template} {
    set script ""
    set pos 0
    foreach pair [regexp -line -all -inline -indices {^%.*$} $template] {
        lassign $pair from to
        set str [string range $template $pos [expr {$from - 2}]]
        if {$str ne ""} {
            append script "$command \[" [list subst $str\n] \]\n
        }
        append script [string range $template [expr {$from + 1}] $to]\n
        set pos [expr {$to + 2}]
    }
    set str [string range $template $pos end]
    if {$str ne ""} {
        append script "$command \[" [list subst $str] \]
    }
    uplevel 1 $script
}

# Get a line of data from a channel.
proc wibble::getline {chan} {
    while {1} {
        if {[chan gets $chan line] >= 0} {
            return $line
        } elseif {[chan pending input $chan] > 4096} {
            if {[chan gets $chan line] >= 0} {
                return $line
            } else {
                error "line length greater than 4096"
            }
        } elseif {[chan eof $chan]} {
            chan close $chan
            return -level [info level]
        } else {
            yield
        }
    }
}

# Get a block of data from a channel.
proc wibble::getblock {chan size} {
    while {1} {
        set chunklet [chan read $chan $size]
        set size [expr {$size - [string length $chunklet]}]
        append chunk $chunklet
        if {$size == 0} {
            return $chunk
        } elseif {[chan eof $chan]} {
            chan close $chan
            return -level [info level]
        } else {
            yield
        }
    }
}

# Decode hexadecimal URL encoding.
proc wibble::unhex {str} {
    set pos 0
    while {[regexp -indices -start $pos {%([[:xdigit:]]{2})} $str range code]} {
        set char [binary format H2 [string range $str {*}$code]]
        set str [string replace $str {*}$range $char]
        set pos [expr {[lindex $range 0] + 1}]
    }
    return $str
}

# Advance to the next zone handler using the specified request/response list.
proc wibble::nexthandler {args} {
    return -level 2 $args
}

# Send a response to the client.
proc wibble::sendresponse {response} {
    return -level 2 [list $response]
}

# Register a zone handler.
proc wibble::handle {zone command args} {
    variable zones
    dict lappend zones $zone [list $command $args]
}

# Get an HTTP request from a client.
proc wibble::getrequest {chan peerhost peerport} {
    # The HTTP header uses CR/LF line breaks.
    chan configure $chan -translation crlf

    # Parse the first line.
    regexp {^\s*(\S*)\s+(\S*)\s+(.*?)\s*$} [getline $chan] _ method uri protocol
    regexp {^([^?]*)(\?.*)?$} $uri _ path query
    set path [regsub -all {(?:/|^)\.(?=/|$)} [unhex $path] /]
    while {[regexp -indices {(?:/[^/]*/+|^[^/]*/+|^)\.\.(?=/|$)} $path range]} {
        set path [string replace $path {*}$range ""]
    }
    set path [regsub -all {//+} /$path /]

    # Start building the request structure.
    set request [dict create socket $chan peerhost $peerhost peerport\
        $peerport method $method uri $uri path $path protocol $protocol\
        header {} rawheader {} query {} rawquery $query]

    # Parse the headers.
    while {[set line [getline $chan]] ne ""} {
        dict lappend request rawheader $line
        if {[regexp {^\s*([^:]*)\s*:\s*(.*?)\s*$} $line _ key val]
         || ([info exists key] && [regexp {^\s*(.*?)\s*$} $line _ val])} {
            set key [string tolower $key]
            if {[dict exists $request header $key]} {
                set val [dict get $request header $key]\n$val
            }
            dict set request header $key $val
        }
    }

    # Parse the query string.
    foreach elem [split [string range $query 1 end] &] {
        regexp {^([^=]*)(?:=(.*))?$} $elem _ key val
        dict set request query [unhex [string map {+ " "} $key]]\
                               [unhex [string map {+ " "} $val]]
    }

    # Get the request body, if there is one.
    if {$method in {POST PUT}} {
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
        dict set request content $data
    }

    return $request
}

# Get a response from the zone handlers.
proc wibble::getresponse {request} {
    variable zones
    set state [list $request [dict create status 500 content "Zone error\n"]]
    dict set fallback status 501
    dict set fallback content "not implemented: [dict get $request uri]\n"
    dict set fallback header content-type text/plain

    # Process all zones.
    dict for {prefix handlers} $zones {
        set match $prefix
        if {[string index $match end] ne "/"} {
            append match /
        }

        # Process all handlers in this zone.
        foreach handler $handlers {
            lassign $handler command options

            # Try all request/response pairs against this handler.
            set i 0
            foreach {request response} $state {
                # Skip this request if it's not for the current zone.
                set path [dict get $request path]
                if {$path ne $prefix && ![string equal\
                        -length [string length $match] $match $path]} {
                    continue
                }

                # Inject a few extra keys into the request dict.
                dict set request prefix $prefix
                dict set request suffix [string range $path\
                                        [string length $prefix] end]
                if {[dict exists $options root]} {
                    dict set request fspath\
                        [dict get $options root]/[dict get $request suffix]
                }
                set request [dict merge $request $options]

                # Invoke the handler and process its outcome.
                set outcome [{*}$command $request $response]
                if {[llength $outcome] == 1} {
                    # A response has been obtained.  Return it.
                    return [lindex $outcome 0]
                } elseif {[llength $outcome] % 2 == 0} {
                    # Filter out extra keys from the new request dicts.
                    for {set j 0} {$j < [llength $outcome]} {incr j 2} {
                        lset outcome $j [dict remove [lindex $outcome $j]\
                                prefix suffix fspath {*}[dict keys $options]]
                    }

                    # Update the state tree and continue processing.
                    set state [lreplace $state $i $i+1 {*}$outcome]
                } else {
                    error "invalid zone handler outcome"
                }
                incr i 2
            }
        }
    }

    # Return 501 as default response.
    return $fallback
}

# Main connection processing loop.
proc wibble::process {socket peerhost peerport} {
    try {
        chan configure $socket -blocking 0
        while {1} {
            # Get request from client, then formulate a response to the reqeust.
            set request [getrequest $socket $peerhost $peerport]
            set response [getresponse $request]

            # Get the content size.
            if {[dict exists $response contentfile]} {
                set size [file size [dict get $response contentfile]]
                if {[dict get $request method] ne "HEAD"} {
                    # Open the channel now, to catch errors early.
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
            dict for {key val} [dict get $response header] {
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
                    # Send response content from a file.
                    chan seek $file $begin
                    chan copy $file $socket -size [expr {$end - $begin + 1}]
                    chan close $file
                } elseif {[dict exists $response content]} {
                    # Send buffered response content.
                    chan puts -nonewline $socket [string range\
                            [dict get $response content] $begin $end]
                }
            }

            # Flush the outgoing buffer.
            chan flush $socket
        }
    } on error {"" options} {
        # Log errors and report them to the client, if possible.
        variable errorcount
        incr errorcount
        set message "*** INTERNAL SERVER ERROR (BEGIN #$errorcount) ***\n"
        append message "time: [clock format [clock seconds]]\n"
        append message "address: $peerhost\n"
        if {[info exists request]} {
            dict for {key val} $request {
                if {$key eq "content" && [string length $val] > 256} {
                    append message "request $key (len=[string length $val])\n"
                } elseif {$key in {header query}} {
                    dict for {subkey subval} $val {
                        append message "request $key $subkey: $subval\n"
                    }
                } else {
                    append message "request $key: $val\n"
                }
            }
        }
        append message "errorinfo: [dict get $options -errorinfo]\n"
        append message "*** INTERNAL SERVER ERROR (END #$errorcount) ***\n"
        log $message
        catch {
            set message [encoding convertto iso8859-1 $message]
            chan configure $socket -translation crlf
            chan puts $socket "HTTP/1.1 500 Internal Server Error"
            chan puts $socket "Content-Type: text/plain; charset=utf-8"
            chan puts $socket "Content-Length: [string length $message]"
            chan puts $socket "Connection: close"
            chan puts $socket ""
            chan configure $socket -translation binary
            chan puts -nonewline $socket $message
        }
    } finally {
        catch {chan close $socket}
    }
}

# Accept an incoming connection.
proc wibble::accept {socket peerhost peerport} {
    chan event $socket readable [namespace code $socket]
    coroutine $socket process $socket $peerhost $peerport
}

# Listen for incoming connections.
proc wibble::listen {port} {
    socket -server [namespace code accept] $port
}

# Log an error.  Feel free to replace this procedure as needed.
proc wibble::log {message} {
    chan puts -nonewline stderr $message
}

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
    wibble::handle / dirlist root $root
    wibble::handle / notfound

    # Start a server and enter the event loop.
    catch {
        wibble::listen 8080
        vwait forever
    }
}

# vim: set sts=4 sw=4 tw=80 et ft=tcl: