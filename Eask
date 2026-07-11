;; -*- mode: eask; lexical-binding: t -*-

(package "disco"
         "0.1.0"
         "Discord client")

(website-url "https://github.com/0WD0/disco.el")
(keywords "comm")

(package-file "disco.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "plz" "0.8")
(depends-on "websocket" "1.16")
(depends-on "transient" "0.5.0")
(depends-on "appkit" :file "../appkit.el")

(script "refresh-appkit" "eask -f install-file ../appkit.el")
(script "test" "eask test ert test/*.el")
(script "test-local" "eask -f install-file ../appkit.el && eask test ert test/*.el")
