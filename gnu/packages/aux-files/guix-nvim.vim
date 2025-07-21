" This appends all applicable nvim paths to the end of packagepath.  Once we
" have told vim the packagepath nvim will add it to the runtimepath for us.
for directory in ["/run/current-system/profile", $HOME . "/.guix-profile", $HOME ."/.guix-home/profile", $GUIX_PROFILE, $GUIX_ENVIRONMENT]
    let vimplugins = directory . "/share/nvim/site"
    if isdirectory(vimplugins)
        let &pp = join([&pp,vimplugins], ',')
    endif
endfor
