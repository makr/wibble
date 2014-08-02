# Interpret tclhttpd style template files.
proc ::wibble::zone::tclhttpd {state} {
    if {[file readable [dict get $state options fspath].tml]} {
        set fin [open [dict get $state options $fspath].tml]]
        set data [read $fin]
        close $fin
        dict set state response status 200
        dict set state response header content-type "" text/html
        dict set state response content [subst $data]
        sendresponse [dict get $state response]
    }
}

::wibble::handle / tclhttpd root $root
