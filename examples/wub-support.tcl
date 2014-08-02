# Some Wub domains won't work, as they require capabilities that wibble doesn't
# provide, but many should. You'd have to construct the Wub domain with
# appropriate arguments, then in the wibble::handle definition, pass an option
# "domain $domain"
proc ::wibble::zone::wub {state} {
    dict with state request {}

    # remap some important request fields
    dict set r -uri $uri
    dict set r -clientheaders [dict keys $state request]
    set r [dict merge $r [dict get $state request] [Url parse $uri 1]]

    try {
        {*}$domain do $r
    } on error {r options} {
        return
    }

    # remap some important result fields
    set response [dict remove $r [dict keys $r -*]]
    dict set response content [dict get $r -content]
    dict set response status [dict get $r -code]
    sendresponse $response
}
