# This zone handler gives a 403 Forbidden when the client requests a file
# whose name is in a list of glob-style patterns.
proc ::wibble::zone::rejectname {state} {
    dict with state request {}; dict with state options {}
    set name [file tail $path]
    foreach pattern $patterns {
        if {[string match $pattern $name]} {
            forbidden $state
        }
    }
}

::wibble::handle / rejectname patterns {*.tmpl *.script}
