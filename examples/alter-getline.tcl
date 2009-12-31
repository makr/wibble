# How about doing the chan gets before the chan pending?
proc wibble::getline {chan} {
    while {1} {
        if {[chan gets $chan line] >= 0} {
            # Got a whole line, may be more than 4096, but that's ok
            return $line
        } elseif {[chan blocked $chan]} {
            # Partial line, see if max length exceeded
            if {[chan pending input $chan] > 4096} {
                # Be anal. Do a gets again since data might have come in
                # between the chan gets and the chan pending calls.
                # For example, there are 4000 bytes without crlf when
                # the chan gets was called. Then another 1000 bytes
                # arrive with a crlf at the front before the chan pending
                # call. The line length limit is not exceeded in that
                # case even through chan pending returns > 4096.
                if {[chan gets $chan line] >= 0} {
                    return $line
                } else {
                    error "line length greater than 4096"
                }
            } else {
                # Incomplete line, but not exceeding max length. Wait for more.
                yield
            }
        } else {
            # EOF
            chan close $chan
            return -level [info level]
        }
    }
}
