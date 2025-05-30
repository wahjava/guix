;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Carmine Margiotta <email@cmargiotta.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu home services hyprland)
  #:use-module (gnu packages wm)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (hyprland-extension
	    hyprland-configuration
	    binding
	    bindings
	    block
	    monitor
	    submap
	    home-hyprland-service-type))

;;; Commentary:
;;;
;;; A Guix Home service to configure Hyprland, an highly customizabile dynamic
;;; tiling Wayland compositor
;;;
;;; Code:

;;;
;;; Definition of configurations.
;;;

;;; Entry inside a 'block' configuration
;;; allowed formats: (symbol string) (symbol number) (symbol boolean)
;;; (symbol block-entries)
;;; A block entry can contain a list of block entries, effectively allowing
;;; nested blocks
(define (block-entry? data)
  (or (null? data)
      (match data
        (((? symbol?)
          (or (? string?)
              (? number?)
              (? boolean?)
              (? block-entries?)))
         #t))))

;;; A block entry will be serialized as an indented hyprlang
;;; statement, nested blocks are allowed
(define (serialize-block-entry value tabs)
  (string-append
   (or (match value
         (() "")
         (((? symbol? name) value)
          (let ((indent (make-string (max 0 tabs) #\tab)))
            (string-append
             indent
             (symbol->string name)
             (match value
               ((? string? v)
                (string-append " = " v))
               ((? number? v)
                (string-append " = " (number->string v)))
               ((? boolean? v)
                (if v " = true" " = false"))
               ((? block-entries? v)
                (string-append " {\n"
                               (serialize-block-entries #f v (+ tabs 1))
                               indent "}")))
             "\n")))
         ((_)
          #f)) "\n")))

;;; List of block entries
(define (block-entries? data)
  (every block-entry? data))

;;; An executable (a target for the exec action) can be a string or a gexp
(define (executable? value)
  (or (string? value)
      (gexp? value)))

;;; A list of valid executables
(define (executable-list? values)
  (every executable? values))

;;; Block sub-configuration (a container of block entries)
(define-configuration block
  (entries (block-entries '()) "Block entries"
           (serializer (λ (name value)
                         (serialize-block-entries name value 1)))))

(define (serialize-block name block)
  #~(string-append #$(symbol->string name) " {\n"
                   #$(if (null? block) ""
                         (serialize-block-entries name (block-entries block) 1))
                   "\n}\n"))

;;; Monitor sub-configuration
(define-configuration monitor
  (name (string "") "Monitor's name"
        empty-serializer)
  (resolution (string "preferred") "Monitor's resolution"
              empty-serializer)
  (position (string "auto") "Monitor's position"
            empty-serializer)
  (scale (string "auto") "Monitor's scale"
         empty-serializer)
  (transform (string "") "Monitor's transform"
             empty-serializer))

(define (serialize-monitor _ monitor)
  #~(string-append
     "monitor = "
     #$(monitor-name monitor) ", "
     #$(monitor-resolution monitor) ", "
     #$(monitor-position monitor) ", "
     #$(monitor-scale monitor) ", "
     #$(let ((transform (monitor-transform monitor)))
          (if (string-null? transform)
              "\n"
              (string-append ", transform, " transform "\n")))))

;;; List of monitors definition
(define (monitors? arg)
  (every monitor? arg))

;;; Environment variable
(define-configuration env
  (name (string) "Environemnt variable's name"
        empty-serializer)
  (value (string) "Environment variable's value"
         empty-serializer))

(define (serialize-env _ e)
  #~(string-append
     "env = "
     #$(env-name e) ", "
     #$(env-value e) "\n"))

;;; List of environment variables
(define (env-list? arg)
  (every env? arg))

;;; List of strings
(define (string-list? arg)
  (every string? arg))

;;; Binding sub-configuration
(define-configuration binding
  (flags (string "")
         "Bind flags https://wiki.hyprland.org/Configuring/Binds/"
         empty-serializer)
  (mod (string "$mod") "Mod key"
       empty-serializer)
  (shift? (boolean #f) "If mod is shifted"
          empty-serializer)
  (alt? (boolean #f) "If alt has to be pressed"
        empty-serializer)
  (ctrl? (boolean #f) "If control has to be pressed"
         empty-serializer)
  (super? (boolean #f) "If super has to be pressed"
          empty-serializer)
  (key (string) "Binding main key"
       empty-serializer)
  (action (string "exec") "Binding action"
          empty-serializer)
  (args (executable "") "Binding action's args"
        empty-serializer))

(define (serialize-binding name b)
  #~(string-append "bind" #$(binding-flags b) " = "
                   #$(binding-mod b)
                   #$(if (binding-shift? b) " SHIFT" "")
                   #$(if (binding-alt? b) " ALT" "")
                   #$(if (binding-ctrl? b) " CTRL" "")
                   #$(if (binding-super? b) " SUPER" "")
                   ", "
                   #$(binding-key b) ", "
                   #$(binding-action b)
                   #$(let ((args (binding-args b)))
                        (if (string? args)
                          (if (string-null? args) "\n"
                              (string-append ", " args "\n"))
                          #~(string-append ", "
                                           #$(serialize-executable name (binding-args b))
                                           "\n")))))

(define (raw-config? value)
  (string? value))

(define (serialize-raw-config _ value)
  (string-append value "\n"))

;;; List of bindings
(define (binding-list? value)
  (every binding? value))

(define (serialize-binding-list name n)
  #~(string-append
     #$@(map (λ (b)
	       (serialize-binding name b))
             n)))

;;; Submap configuration
(define-configuration submap
  (name (string) "Submap name"
        empty-serializer)
  (bindings (binding-list)
            "Bindings available only while this submap is active")
  (escape (binding (binding
                    (mod "")
                    (key "escape")
                    (action "submap")
                    (args "reset")))
          "Binding used to go back to the global submap"))

(define (serialize-submap name s)
  #~(string-append
     "submap = "
     #$(submap-name s) "\n"
     #$(serialize-binding-list name (submap-bindings s))
     #$(serialize-binding name (submap-escape s))
     "submap = reset\n"))

;;; List of submaps
(define (submap-list? v)
  (every submap? v))

;;; Binding block sub-configuration
(define-configuration bindings
  (main-mod (string "") "Main mod bound to $mod"
            empty-serializer)
  (binds (binding-list '()) "Bindings"))

(define (serialize-bindings name b)
  #~(string-append
     "\n$mod = "
     #$(bindings-main-mod b) "\n\n"
     #$(serialize-binding-list name (bindings-binds b))))

;;;
;;; Serialization functions.
;;;
(define (serialize-submap-list name submaps)
  #~(string-append
     #$@(map (λ (v) (serialize-submap name v)) submaps)))

(define (serialize-env-list name env)
  #~(string-append
     #$@(map (λ (v) (serialize-env name v)) env)))

;;; String lists will be serialized as name = value\n
(define (serialize-string-list name values)
  (apply string-append
         (map (λ (w)
                (string-append (symbol->string name) " = " w "\n")) values)))

;;; Gexp executables will be serialized on a program-file
(define (serialize-executable name value)
  (if (string? value) value
      (program-file (symbol->string name) value
                    #:module-path %load-path)))

;;; Lists serializers
(define (serialize-block-entries _ entries level)
  (apply string-append
         (map (λ (e)
                (serialize-block-entry e level)) entries)))

(define (serialize-monitors name monitors)
  #~(string-append #$@(map (λ (m)
                             (serialize-monitor name m))
                           monitors)))

(define (serialize-executable-list name values)
  #~(apply string-append
           (map (λ (w)
                  (string-append #$(symbol->string name) " = " w "\n"))
                '#$(map (λ (v)
                          (serialize-executable name v)) values))))

;;; Hyprland full configuration
(define-configuration hyprland-configuration
  (package (package hyprland) "Hyprland package to use"
           empty-serializer)
  (monitors (monitors (list (monitor))) "Monitors definition")
  (exec-once (executable-list '()) "Command to exec once")
  (exec (executable-list '()) "Command to automatically exec")
  (general (block (block)) "General configuration variables")
  (decoration (block (block)) "Decoration configuration variables")
  (animations (block (block)) "Animation configuration variables")
  (workspace (string-list '()) "Workspaces settings")
  (windowrule (string-list '()) "Window rules (v2)")
  (dwindle (block (block)) "Dwindle layout settings")
  (master (block (block)) "Master layout settings")
  (misc (block (block)) "Misc settings")
  (input (block (block)) "Input settings")
  (gestures (block (block)) "Gestures settings")
  (environment (env-list '()) "Environment variables")
  (bindings (bindings (bindings)) "Bindings configuration")
  (submaps (submap-list '()) "Submap configuration")
  (extra-config (raw-config "") "Extra config"))

;;; Hyprland configuration extension for other services
;;; External services can add new exec entries or new bindings
(define-configuration hyprland-extension
  (exec-once (executable-list '())
             "Commands to be executed with hyprland once")
  (exec (executable-list '()) "Commands to be executed with hyprland")
  (bindings (binding-list '()) "Extra binds")
  (no-serialization))

;;;
;;; Default settings and useful constants.
;;;
(define-public %default-hyprland-env
  (list
   (env (name "XCURSOR_SIZE")
        (value "24"))
   (env (name "HYPRCURSOR_SIZE")
        (value "24"))))

(define-public %default-hyprland-windowrule
  '("suppressevent maximize, class:.*"
    "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"))

(define-public %default-hyprland-general
  (block (entries '((gaps_in 5)
                    (gaps_out 20)
                    (border_size 2)
                    (col.active_border "rgba(33ccffee) rgba(00ff99ee) 45deg")
                    (col.inactive_border "rgba(595959aa)")
                    (resize_on_border #f)
                    (allow_tearing #f)
                    (layout "dwindle")))))

(define-public %default-hyprland-decoration
  (block (entries '((rounding 10)
                    (rounding_power 2)
                    (active_opacity 1.0)
                    (inactive_opacity 1.0)

                    (shadow ((enabled #t)
                             (range 4)
                             (render_power 3)
                             (color "rgba(1a1a1aee)")))
                    (blur ((enabled #t)
                           (size 3)
                           (passes 1)
                           (vibrancy 0.1696)))))))

(define-public %default-hyprland-animations
  (block (entries
          '((enabled #t)
            (bezier "easeOutQuint,0.23,1,0.32,1")
            (bezier "easeInOutCubic,0.65,0.05,0.36,1")
            (bezier "linear,0,0,1,1")
            (bezier "almostLinear,0.5,0.5,0.75,1.0")
            (bezier "quick,0.15,0,0.1,1")
            (animation "global, 1, 10, default")
            (animation "border, 1, 5.39, easeOutQuint")
            (animation "windows, 1, 4.79, easeOutQuint")
            (animation "windowsIn, 1, 4.1, easeOutQuint, popin 87%")
            (animation "windowsOut, 1, 1.49, linear, popin 87%")
            (animation "fadeIn, 1, 1.73, almostLinear")
            (animation "fadeOut, 1, 1.46, almostLinear")
            (animation "fade, 1, 3.03, quick")
            (animation "layers, 1, 3.81, easeOutQuint")
            (animation "layersIn, 1, 4, easeOutQuint, fade")
            (animation "layersOut, 1, 1.5, linear, fade")
            (animation "fadeLayersIn, 1, 1.79, almostLinear")
            (animation "fadeLayersOut, 1, 1.39, almostLinear")
            (animation "workspaces, 1, 1.94, almostLinear, fade")
            (animation "workspacesIn, 1, 1.21, almostLinear, fade")
            (animation "workspacesOut, 1, 1.94, almostLinear, fade")))))

(define-public %default-hyprland-misc
  (block (entries '((force_default_wallpaper -1)
                    (disable_hyprland_logo #f)))))

(define-public %default-hyprland-dwindle
  (block (entries '((pseudotile #t)
                    (preserve_split #t)))))

(define-public %default-hyprland-master
  (block (entries '((new_status "master")))))

(define-public %default-hyprland-input
  (block (entries '((kb_layout "us")
                    (follow_mouse 1)
                    (sensitivity 0)
                    (touchpad ((natural_scroll #f)))))))

(define-public %default-hyprland-gestures
  (block (entries '((workspace_swipe #f)))))

(define-public %default-hyprland-bindings
  (bindings (main-mod "SUPER")
            (binds `(,(binding (key "Q")
                               (action "exec")
                               (args "kitty"))
                     ,(binding (key "C")
                               (action "killactive"))
                     ,(binding (key "M")
                               (action "exit"))
                     ,(binding (key "E")
                               (action "exec")
                               (args "dolphin"))
                     ,(binding (key "V")
                               (action "togglefloating"))
                     ,(binding (key "R")
                               (action "exec")
                               (args "wofi --show dmenu"))
                     ;; Dwindle layout
                     ,(binding (key "P")
                               (action "pseudo"))
                     ,(binding (key "J")
                               (action "togglesplit"))
                     ;; Move focus with arrow keys
                     ,(binding (key "left")
                               (action "movefocus")
                               (args "l"))
                     ,(binding (key "right")
                               (action "movefocus")
                               (args "r"))
                     ,(binding (key "up")
                               (action "movefocus")
                               (args "u"))
                     ,(binding (key "down")
                               (action "movefocus")
                               (args "d"))
                     ;; Switch workspaces
                     ,@(map (lambda (index)
                              (binding (key (number->string index))
                                       (action "workspace")
                                       (args (number->string index))))
                            (iota 10))
                     ;; Move active window to workspace
                     ,@(map (lambda (index)
                              (binding (shift? #t)
                                       (key (number->string index))
                                       (action "movetoworkspace")
                                       (args (number->string index))))
                            (iota 10))
                     ;; Scratchpad
                     ,(binding (key "S")
                               (action "togglespecialworkspace")
                               (args "magic"))
                     ,(binding (key "S")
                               (shift? #t)
                               (action "movetoworkspace")
                               (args "special:magic"))
                     ;; Scroll workspaces with mod + scroll
                     ,(binding (key "mouse_down")
                               (action "workspace")
                               (args "e+1"))
                     ,(binding (key "mouse_up")
                               (action "workspace")
                               (args "e-1"))
                     ;; Move/resize with mouse
                     ,(binding (flags "m")
                               (key "mouse:272")
                               (action "movewindow"))
                     ,(binding (flags "m")
                               (key "mouse:273")
                               (action "resizewindow"))
                     ;; Multimedia keys
                     ,(binding
                       (key "XF86AudioRaiseVolume")
                       (action "exec")
                       (args
                        "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"))
                     ,(binding
                       (key "XF86AudioLowerVolume")
                       (action "exec")
                       (args
                        "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"))
                     ,(binding
                       (key "XF86AudioMute")
                       (action "exec")
                       (args "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
                     ,(binding
                       (key "XF86AudioMicMute")
                       (action "exec")
                       (args "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"))
                     ,(binding
                       (key "XF86MonBrightnessUp")
                       (action "exec")
                       (args "brightnessctl s 10%+"))
                     ,(binding
                       (key "XF86MonBrightnessDown")
                       (action "exec")
                       (args "brightnessctl s 10%-"))
                     ,(binding
                       (key "XF86AudioNext")
                       (action "exec")
                       (args "playerctl next"))
                     ,(binding
                       (key "XF86AudioPause")
                       (action "exec")
                       (args "playerctl play-pause"))
                     ,(binding
                       (key "XF86AudioPlay")
                       (action "exec")
                       (args "playerctl play-pause"))
                     ,(binding
                       (key "XF86AudioPrev")
                       (action "exec")
                       (args "playerctl previous"))))))

(define-public %default-hyprland-configuration
  (hyprland-configuration (general %default-hyprland-general)
                          (decoration %default-hyprland-decoration)
                          (animations %default-hyprland-animations)
                          (environment %default-hyprland-env)
                          (master %default-hyprland-master)
                          (windowrule %default-hyprland-windowrule)
                          (misc %default-hyprland-misc)
                          (input %default-hyprland-input)
                          (dwindle %default-hyprland-dwindle)
                          (gestures %default-hyprland-gestures)
                          (bindings %default-hyprland-bindings)))

;;;
;;; Useful scripts
;;;

;;; Obtained with string-hash-ci on the default hyprland.conf
;;; Maintainers can find the hash upgrades on home service logs too
(define %default-configuration-hash 415437361418070137)

;;; Reload the first instance of hyprland to automatically load the new
;;; configuration. If the package's default configuration changes, display a
;;; notification in Hyprland asking for a review of the service's provided defaults.
(define (hyprland-reload config)
  (with-imported-modules
      (source-module-closure
       '((ice-9 textual-ports)))
    #~(begin
        (use-modules (ice-9 textual-ports))
        (display "Reloading hyprland configuration...\n")
        (system* #$(file-append (hyprland-configuration-package config)
			        "/bin/hyprctl")
		 "--instance" "0" "reload")
        (let ((hash (call-with-input-file
		        #$(file-append (hyprland-configuration-package config)
				       "/share/hypr/hyprland.conf")
		      (lambda (config)
                        (string-hash-ci (get-string-all config))))))
	  (if (not (= hash
		      #$%default-configuration-hash))
	      (begin
	        (display (string-append
			  "New hyprland default configuration detected, "
                          "hash value: "
			  (number->string hash)
			  "\n"))
	        (system* #$(file-append
                            (hyprland-configuration-package config)
			    "/bin/hyprctl")
			 "--instance"
			 "0"
			 "notify"
			 "0" ; this is a warning
			 "20000" ; 10s duration
			 "0" ; default color
			 "Hyprland's default configuration file has changed, and its \
Guix service may be out of sync. Please file a bug via bug-guix@gnu.org.")))))))

;;;
;;; Definition of the Home Service.
;;;

(define-public home-hyprland-service-type
  (service-type (name 'home-hyprland-config)
                (description "Configure Hyprland by providing a file
@file{~/.config/hypr/hyprland.conf}.")
                (compose
                 (λ (extensions)
                   (let ((flatten
                          (λ (lst)
                            (let loop ((lst lst) (acc '()))
                              (cond ((null? lst) acc)
                                    ((pair? lst) (loop (car lst)
                                                       (loop (cdr lst) acc)))
                                    (else (cons lst acc)))))))
                     (hyprland-extension
                      (exec-once
                       (flatten (map hyprland-extension-exec-once extensions)))
                      (exec (flatten (map hyprland-extension-exec
                                          extensions)))
                      (bindings (flatten (map hyprland-extension-bindings
                                              extensions)))))))
                (extend
                 (λ (config rules)
                   (hyprland-configuration
                    (inherit config)
                    (exec-once
                     (append
                      (hyprland-configuration-exec-once
                       config)
                      (hyprland-extension-exec-once
                       rules)))
                    (exec
                     (append (hyprland-configuration-exec
                              config)
                             (hyprland-extension-exec
                              rules)))
                    (bindings (bindings
                               (inherit
                                (hyprland-configuration-bindings
                                 config))
                               (binds (append
                                       (bindings-binds
                                        (hyprland-configuration-bindings
                                         config))
                                       (hyprland-extension-bindings
                                        rules))))))))
                (extensions
                 (list (service-extension
                        home-activation-service-type
                                        ; Trigger hyprctl reload after
                                        ; a new config has been applied
                        hyprland-reload)
                       (service-extension
                        home-profile-service-type
                        (λ (config)
                          `(,(hyprland-configuration-package
                              config))))
                       (service-extension
                        home-xdg-configuration-files-service-type
                        (λ (c)
                          `(("hypr/hyprland.conf"
                             ,(mixed-text-file
                               "hyprland-cfg"
                               (serialize-configuration
                                c
                                hyprland-configuration-fields))))))))
                (default-value %default-hyprland-configuration)))
