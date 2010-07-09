# Some Wub domains won't work, as they require capabilities that wibble doesn't
# provide, but many should. You'd have to construct the Wub domain with
# appropriate arguments, then in the wibble::handle definition, pass an option
# "domain $domain"
proc wibble::wub {request response} {
    dict with request {}
    # remap some important request fields
    dict set r -uri $uri
    dict set r -clientheaders [dict keys $request]
    set r [dict merge $r $request [Url parse $uri 1]]

    try {
        {*}$domain do $r
    } on error {r options} {
        return [nexthandler $request $response]
    }

    # remap some important result fields
    dict set response content [dict get $r -content]
    dict set response status [dict get $r -code]
    sendresponse [dict merge $response [dict remove $r [dict keys $r -*]]]
}
