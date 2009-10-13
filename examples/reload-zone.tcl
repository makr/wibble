# a '''reload''' zone handler
proc reload {request response} {
    set wibble::zones {}
    foreach script [dict get $request scripts] {
        uplevel #0 [list source $script]
    }
    dict set response status 200
    dict set response header content-type text/plain
    dict set response content "Server reloaded.  Have a nice day.\n"
    wibble::sendresponse $response
}
wibble::handle /reload reload scripts [list [info script]]

