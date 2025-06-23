;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lapearldot <lapearldot@disroot.org>
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

(define-module (gnu home services helix)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix build-system)
  #:use-module (gnu home services)

  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tree-sitter)

  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)

  #:export (
      ;; a a-list of all tree sitter grammars
       %all-guix-tree-sitter-grammars
      ;; a a-list of all packaged helix queries
       %shipped-helix-query-folders
      ;; a a-list of all packaged helix themes
       %shipped-helix-themes

       ;; a gexp to update the query a-list
       get-query-list
       ;; a gexp to update the theme a-list
       get-theme-list

       <helix-configuration>
       helix-configuration
       make-helix-configuration
       helix-configuration?
       this-helix-configuration
       helix-configuration-grammars
       helix-configuration-queries
       helix-configuration-themes

       home-helix-service-type))


; To allow the user to configure an arbitrary alist of queries and themes, these are extracted from the helix package
; Doing so at configure time would lead to behaviour dependant on the state of the helix package and incur an unexpected pre configuration calculation
; Instead the files are pulled in by functions defined here by the functions: `get-query-list` `get-theme-list`
; In my knowledge, resolving to some calculated package set is not possible with: guix import + refresh

(define helix-source
  (package-source helix))
(define grammars
  (list tree-sitter-html
        tree-sitter-javascript
        tree-sitter-typescript
        tree-sitter-bibtex
        tree-sitter-css
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-cmake
        tree-sitter-devicetree
        tree-sitter-elixir
        tree-sitter-heex
        tree-sitter-bash
        tree-sitter-c-sharp
        tree-sitter-dockerfile
        tree-sitter-erlang
        tree-sitter-elm
        tree-sitter-gomod
        tree-sitter-go
        tree-sitter-haskell
        tree-sitter-hcl
        tree-sitter-java
        tree-sitter-json
        tree-sitter-julia
        tree-sitter-kdl
        tree-sitter-ocaml
        tree-sitter-php
        tree-sitter-prisma
        tree-sitter-python
        tree-sitter-r
        tree-sitter-ron
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-ungrammar
        tree-sitter-clojure
        tree-sitter-markdown
        tree-sitter-markdown-gfm
        tree-sitter-matlab
        tree-sitter-meson
        tree-sitter-nix
        tree-sitter-org
        tree-sitter-scheme
        tree-sitter-sway
        tree-sitter-racket
        tree-sitter-plantuml
        tree-sitter-latex
        tree-sitter-lua
        tree-sitter-scala
        tree-sitter-tlaplus
        tree-sitter-kotlin
        tree-sitter-awk
        tree-sitter-verilog
        tree-sitter-vhdl))

(define %all-guix-tree-sitter-grammars
  (let ((prefix-length (string-length "tree-sitter-")))
    (map (lambda (grammar-package)
           (let ((name (string-drop (package-name grammar-package)
                                    prefix-length)))
             (cons name grammar-package))) grammars)))

(define (get-runtime-file-sexps runtime-dir name-from-file-name)
  #~(begin
      (use-modules (ice-9 ftw))
      (write `(list ,@(map (lambda (file-name)
                             `(cons ,(eval `(#$name-from-file-name
                                             ,file-name)
                                           (interaction-environment))
                                    (file-append helix-source
                                                 ,(string-append "/runtime/"
                                                                 #$runtime-dir
                                                                 "/" file-name))))
                           (scandir (string-append #$helix-source "/runtime/"
                                                   #$runtime-dir)
                                    (lambda (name)
                                      (not (member name
                                                   (list "." ".."))))))))))
(define get-query-list
  (get-runtime-file-sexps "queries"
                          '(lambda (q)
                             q)))
(define get-theme-list
  (get-runtime-file-sexps "themes"
                          '(lambda (q)
                             (string-drop-right q 5))))

(define %shipped-helix-query-folders
  (list (cons "astro"
              (file-append helix-source "/runtime/queries/astro"))
        (cons "awk"
              (file-append helix-source "/runtime/queries/awk"))
        (cons "bash"
              (file-append helix-source "/runtime/queries/bash"))
        (cons "bass"
              (file-append helix-source "/runtime/queries/bass"))
        (cons "beancount"
              (file-append helix-source "/runtime/queries/beancount"))
        (cons "bibtex"
              (file-append helix-source "/runtime/queries/bibtex"))
        (cons "bicep"
              (file-append helix-source "/runtime/queries/bicep"))
        (cons "blueprint"
              (file-append helix-source "/runtime/queries/blueprint"))
        (cons "c"
              (file-append helix-source "/runtime/queries/c"))
        (cons "cairo"
              (file-append helix-source "/runtime/queries/cairo"))
        (cons "capnp"
              (file-append helix-source "/runtime/queries/capnp"))
        (cons "clojure"
              (file-append helix-source "/runtime/queries/clojure"))
        (cons "cmake"
              (file-append helix-source "/runtime/queries/cmake"))
        (cons "comment"
              (file-append helix-source "/runtime/queries/comment"))
        (cons "common-lisp"
              (file-append helix-source "/runtime/queries/common-lisp"))
        (cons "cpon"
              (file-append helix-source "/runtime/queries/cpon"))
        (cons "cpp"
              (file-append helix-source "/runtime/queries/cpp"))
        (cons "crystal"
              (file-append helix-source "/runtime/queries/crystal"))
        (cons "c-sharp"
              (file-append helix-source "/runtime/queries/c-sharp"))
        (cons "css"
              (file-append helix-source "/runtime/queries/css"))
        (cons "cue"
              (file-append helix-source "/runtime/queries/cue"))
        (cons "d"
              (file-append helix-source "/runtime/queries/d"))
        (cons "dart"
              (file-append helix-source "/runtime/queries/dart"))
        (cons "devicetree"
              (file-append helix-source "/runtime/queries/devicetree"))
        (cons "dhall"
              (file-append helix-source "/runtime/queries/dhall"))
        (cons "diff"
              (file-append helix-source "/runtime/queries/diff"))
        (cons "dockerfile"
              (file-append helix-source "/runtime/queries/dockerfile"))
        (cons "dot"
              (file-append helix-source "/runtime/queries/dot"))
        (cons "dtd"
              (file-append helix-source "/runtime/queries/dtd"))
        (cons "ecma"
              (file-append helix-source "/runtime/queries/ecma"))
        (cons "edoc"
              (file-append helix-source "/runtime/queries/edoc"))
        (cons "eex"
              (file-append helix-source "/runtime/queries/eex"))
        (cons "ejs"
              (file-append helix-source "/runtime/queries/ejs"))
        (cons "elixir"
              (file-append helix-source "/runtime/queries/elixir"))
        (cons "elm"
              (file-append helix-source "/runtime/queries/elm"))
        (cons "elvish"
              (file-append helix-source "/runtime/queries/elvish"))
        (cons "env"
              (file-append helix-source "/runtime/queries/env"))
        (cons "erb"
              (file-append helix-source "/runtime/queries/erb"))
        (cons "erlang"
              (file-append helix-source "/runtime/queries/erlang"))
        (cons "esdl"
              (file-append helix-source "/runtime/queries/esdl"))
        (cons "fish"
              (file-append helix-source "/runtime/queries/fish"))
        (cons "forth"
              (file-append helix-source "/runtime/queries/forth"))
        (cons "fortran"
              (file-append helix-source "/runtime/queries/fortran"))
        (cons "fsharp"
              (file-append helix-source "/runtime/queries/fsharp"))
        (cons "gas"
              (file-append helix-source "/runtime/queries/gas"))
        (cons "gdscript"
              (file-append helix-source "/runtime/queries/gdscript"))
        (cons "gemini"
              (file-append helix-source "/runtime/queries/gemini"))
        (cons "git-attributes"
              (file-append helix-source "/runtime/queries/git-attributes"))
        (cons "git-commit"
              (file-append helix-source "/runtime/queries/git-commit"))
        (cons "git-config"
              (file-append helix-source "/runtime/queries/git-config"))
        (cons "git-ignore"
              (file-append helix-source "/runtime/queries/git-ignore"))
        (cons "git-rebase"
              (file-append helix-source "/runtime/queries/git-rebase"))
        (cons "gleam"
              (file-append helix-source "/runtime/queries/gleam"))
        (cons "glsl"
              (file-append helix-source "/runtime/queries/glsl"))
        (cons "go"
              (file-append helix-source "/runtime/queries/go"))
        (cons "godot-resource"
              (file-append helix-source "/runtime/queries/godot-resource"))
        (cons "gomod"
              (file-append helix-source "/runtime/queries/gomod"))
        (cons "gotmpl"
              (file-append helix-source "/runtime/queries/gotmpl"))
        (cons "gowork"
              (file-append helix-source "/runtime/queries/gowork"))
        (cons "graphql"
              (file-append helix-source "/runtime/queries/graphql"))
        (cons "hare"
              (file-append helix-source "/runtime/queries/hare"))
        (cons "haskell"
              (file-append helix-source "/runtime/queries/haskell"))
        (cons "haskell-persistent"
              (file-append helix-source "/runtime/queries/haskell-persistent"))
        (cons "hcl"
              (file-append helix-source "/runtime/queries/hcl"))
        (cons "heex"
              (file-append helix-source "/runtime/queries/heex"))
        (cons "hosts"
              (file-append helix-source "/runtime/queries/hosts"))
        (cons "html"
              (file-append helix-source "/runtime/queries/html"))
        (cons "hurl"
              (file-append helix-source "/runtime/queries/hurl"))
        (cons "iex"
              (file-append helix-source "/runtime/queries/iex"))
        (cons "ini"
              (file-append helix-source "/runtime/queries/ini"))
        (cons "java"
              (file-append helix-source "/runtime/queries/java"))
        (cons "_javascript"
              (file-append helix-source "/runtime/queries/_javascript"))
        (cons "javascript"
              (file-append helix-source "/runtime/queries/javascript"))
        (cons "jinja"
              (file-append helix-source "/runtime/queries/jinja"))
        (cons "jsdoc"
              (file-append helix-source "/runtime/queries/jsdoc"))
        (cons "json"
              (file-append helix-source "/runtime/queries/json"))
        (cons "jsonnet"
              (file-append helix-source "/runtime/queries/jsonnet"))
        (cons "json5"
              (file-append helix-source "/runtime/queries/json5"))
        (cons "_jsx"
              (file-append helix-source "/runtime/queries/_jsx"))
        (cons "jsx"
              (file-append helix-source "/runtime/queries/jsx"))
        (cons "julia"
              (file-append helix-source "/runtime/queries/julia"))
        (cons "just"
              (file-append helix-source "/runtime/queries/just"))
        (cons "kdl"
              (file-append helix-source "/runtime/queries/kdl"))
        (cons "kotlin"
              (file-append helix-source "/runtime/queries/kotlin"))
        (cons "latex"
              (file-append helix-source "/runtime/queries/latex"))
        (cons "lean"
              (file-append helix-source "/runtime/queries/lean"))
        (cons "ledger"
              (file-append helix-source "/runtime/queries/ledger"))
        (cons "llvm"
              (file-append helix-source "/runtime/queries/llvm"))
        (cons "llvm-mir"
              (file-append helix-source "/runtime/queries/llvm-mir"))
        (cons "llvm-mir-yaml"
              (file-append helix-source "/runtime/queries/llvm-mir-yaml"))
        (cons "lua"
              (file-append helix-source "/runtime/queries/lua"))
        (cons "make"
              (file-append helix-source "/runtime/queries/make"))
        (cons "markdoc"
              (file-append helix-source "/runtime/queries/markdoc"))
        (cons "markdown"
              (file-append helix-source "/runtime/queries/markdown"))
        (cons "markdown.inline"
              (file-append helix-source "/runtime/queries/markdown.inline"))
        (cons "matlab"
              (file-append helix-source "/runtime/queries/matlab"))
        (cons "mermaid"
              (file-append helix-source "/runtime/queries/mermaid"))
        (cons "meson"
              (file-append helix-source "/runtime/queries/meson"))
        (cons "msbuild"
              (file-append helix-source "/runtime/queries/msbuild"))
        (cons "nasm"
              (file-append helix-source "/runtime/queries/nasm"))
        (cons "nickel"
              (file-append helix-source "/runtime/queries/nickel"))
        (cons "nim"
              (file-append helix-source "/runtime/queries/nim"))
        (cons "nix"
              (file-append helix-source "/runtime/queries/nix"))
        (cons "nu"
              (file-append helix-source "/runtime/queries/nu"))
        (cons "nunjucks"
              (file-append helix-source "/runtime/queries/nunjucks"))
        (cons "ocaml"
              (file-append helix-source "/runtime/queries/ocaml"))
        (cons "ocaml-interface"
              (file-append helix-source "/runtime/queries/ocaml-interface"))
        (cons "odin"
              (file-append helix-source "/runtime/queries/odin"))
        (cons "opencl"
              (file-append helix-source "/runtime/queries/opencl"))
        (cons "openscad"
              (file-append helix-source "/runtime/queries/openscad"))
        (cons "org"
              (file-append helix-source "/runtime/queries/org"))
        (cons "pascal"
              (file-append helix-source "/runtime/queries/pascal"))
        (cons "passwd"
              (file-append helix-source "/runtime/queries/passwd"))
        (cons "pem"
              (file-append helix-source "/runtime/queries/pem"))
        (cons "perl"
              (file-append helix-source "/runtime/queries/perl"))
        (cons "php"
              (file-append helix-source "/runtime/queries/php"))
        (cons "po"
              (file-append helix-source "/runtime/queries/po"))
        (cons "pod"
              (file-append helix-source "/runtime/queries/pod"))
        (cons "ponylang"
              (file-append helix-source "/runtime/queries/ponylang"))
        (cons "prisma"
              (file-append helix-source "/runtime/queries/prisma"))
        (cons "protobuf"
              (file-append helix-source "/runtime/queries/protobuf"))
        (cons "prql"
              (file-append helix-source "/runtime/queries/prql"))
        (cons "purescript"
              (file-append helix-source "/runtime/queries/purescript"))
        (cons "python"
              (file-append helix-source "/runtime/queries/python"))
        (cons "qml"
              (file-append helix-source "/runtime/queries/qml"))
        (cons "r"
              (file-append helix-source "/runtime/queries/r"))
        (cons "racket"
              (file-append helix-source "/runtime/queries/racket"))
        (cons "regex"
              (file-append helix-source "/runtime/queries/regex"))
        (cons "rego"
              (file-append helix-source "/runtime/queries/rego"))
        (cons "rescript"
              (file-append helix-source "/runtime/queries/rescript"))
        (cons "rmarkdown"
              (file-append helix-source "/runtime/queries/rmarkdown"))
        (cons "robot"
              (file-append helix-source "/runtime/queries/robot"))
        (cons "ron"
              (file-append helix-source "/runtime/queries/ron"))
        (cons "rst"
              (file-append helix-source "/runtime/queries/rst"))
        (cons "ruby"
              (file-append helix-source "/runtime/queries/ruby"))
        (cons "rust"
              (file-append helix-source "/runtime/queries/rust"))
        (cons "sage"
              (file-append helix-source "/runtime/queries/sage"))
        (cons "scala"
              (file-append helix-source "/runtime/queries/scala"))
        (cons "scss"
              (file-append helix-source "/runtime/queries/scss"))
        (cons "scheme"
              (file-append helix-source "/runtime/queries/scheme"))
        (cons "slint"
              (file-append helix-source "/runtime/queries/slint"))
        (cons "smithy"
              (file-append helix-source "/runtime/queries/smithy"))
        (cons "sml"
              (file-append helix-source "/runtime/queries/sml"))
        (cons "solidity"
              (file-append helix-source "/runtime/queries/solidity"))
        (cons "sql"
              (file-append helix-source "/runtime/queries/sql"))
        (cons "sshclientconfig"
              (file-append helix-source "/runtime/queries/sshclientconfig"))
        (cons "starlark"
              (file-append helix-source "/runtime/queries/starlark"))
        (cons "strace"
              (file-append helix-source "/runtime/queries/strace"))
        (cons "svelte"
              (file-append helix-source "/runtime/queries/svelte"))
        (cons "sway"
              (file-append helix-source "/runtime/queries/sway"))
        (cons "swift"
              (file-append helix-source "/runtime/queries/swift"))
        (cons "tablegen"
              (file-append helix-source "/runtime/queries/tablegen"))
        (cons "task"
              (file-append helix-source "/runtime/queries/task"))
        (cons "templ"
              (file-append helix-source "/runtime/queries/templ"))
        (cons "tfvars"
              (file-append helix-source "/runtime/queries/tfvars"))
        (cons "todotxt"
              (file-append helix-source "/runtime/queries/todotxt"))
        (cons "toml"
              (file-append helix-source "/runtime/queries/toml"))
        (cons "tsq"
              (file-append helix-source "/runtime/queries/tsq"))
        (cons "tsx"
              (file-append helix-source "/runtime/queries/tsx"))
        (cons "twig"
              (file-append helix-source "/runtime/queries/twig"))
        (cons "_typescript"
              (file-append helix-source "/runtime/queries/_typescript"))
        (cons "typescript"
              (file-append helix-source "/runtime/queries/typescript"))
        (cons "t32"
              (file-append helix-source "/runtime/queries/t32"))
        (cons "ungrammar"
              (file-append helix-source "/runtime/queries/ungrammar"))
        (cons "unison"
              (file-append helix-source "/runtime/queries/unison"))
        (cons "uxntal"
              (file-append helix-source "/runtime/queries/uxntal"))
        (cons "v"
              (file-append helix-source "/runtime/queries/v"))
        (cons "vala"
              (file-append helix-source "/runtime/queries/vala"))
        (cons "verilog"
              (file-append helix-source "/runtime/queries/verilog"))
        (cons "vhdl"
              (file-append helix-source "/runtime/queries/vhdl"))
        (cons "vhs"
              (file-append helix-source "/runtime/queries/vhs"))
        (cons "vue"
              (file-append helix-source "/runtime/queries/vue"))
        (cons "wast"
              (file-append helix-source "/runtime/queries/wast"))
        (cons "wat"
              (file-append helix-source "/runtime/queries/wat"))
        (cons "webc"
              (file-append helix-source "/runtime/queries/webc"))
        (cons "wgsl"
              (file-append helix-source "/runtime/queries/wgsl"))
        (cons "wit"
              (file-append helix-source "/runtime/queries/wit"))
        (cons "wren"
              (file-append helix-source "/runtime/queries/wren"))
        (cons "xit"
              (file-append helix-source "/runtime/queries/xit"))
        (cons "xml"
              (file-append helix-source "/runtime/queries/xml"))
        (cons "yaml"
              (file-append helix-source "/runtime/queries/yaml"))
        (cons "yuck"
              (file-append helix-source "/runtime/queries/yuck"))
        (cons "zig"
              (file-append helix-source "/runtime/queries/zig"))))
(define %shipped-helix-themes
  (list (cons "acme"
              (file-append helix-source "/runtime/themes/acme.toml"))
        (cons "adwaita-dark"
              (file-append helix-source "/runtime/themes/adwaita-dark.toml"))
        (cons "amberwood"
              (file-append helix-source "/runtime/themes/amberwood.toml"))
        (cons "autumn_night"
              (file-append helix-source "/runtime/themes/autumn_night.toml"))
        (cons "autumn"
              (file-append helix-source "/runtime/themes/autumn.toml"))
        (cons "ayu_dark"
              (file-append helix-source "/runtime/themes/ayu_dark.toml"))
        (cons "ayu_evolve"
              (file-append helix-source "/runtime/themes/ayu_evolve.toml"))
        (cons "ayu_light"
              (file-append helix-source "/runtime/themes/ayu_light.toml"))
        (cons "ayu_mirage"
              (file-append helix-source "/runtime/themes/ayu_mirage.toml"))
        (cons "base16_default_dark"
              (file-append helix-source
                           "/runtime/themes/base16_default_dark.toml"))
        (cons "base16_default_light"
              (file-append helix-source
                           "/runtime/themes/base16_default_light.toml"))
        (cons "base16_terminal"
              (file-append helix-source "/runtime/themes/base16_terminal.toml"))
        (cons "base16_transparent"
              (file-append helix-source
                           "/runtime/themes/base16_transparent.toml"))
        (cons "bogster_light"
              (file-append helix-source "/runtime/themes/bogster_light.toml"))
        (cons "bogster"
              (file-append helix-source "/runtime/themes/bogster.toml"))
        (cons "boo_berry"
              (file-append helix-source "/runtime/themes/boo_berry.toml"))
        (cons "catppuccin_frappe"
              (file-append helix-source
                           "/runtime/themes/catppuccin_frappe.toml"))
        (cons "catppuccin_latte"
              (file-append helix-source
                           "/runtime/themes/catppuccin_latte.toml"))
        (cons "catppuccin_macchiato"
              (file-append helix-source
                           "/runtime/themes/catppuccin_macchiato.toml"))
        (cons "catppuccin_mocha"
              (file-append helix-source
                           "/runtime/themes/catppuccin_mocha.toml"))
        (cons "cyan_light"
              (file-append helix-source "/runtime/themes/cyan_light.toml"))
        (cons "darcula-solid"
              (file-append helix-source "/runtime/themes/darcula-solid.toml"))
        (cons "darcula"
              (file-append helix-source "/runtime/themes/darcula.toml"))
        (cons "dark_high_contrast"
              (file-append helix-source
                           "/runtime/themes/dark_high_contrast.toml"))
        (cons "dark_plus"
              (file-append helix-source "/runtime/themes/dark_plus.toml"))
        (cons "doom_acario_dark"
              (file-append helix-source
                           "/runtime/themes/doom_acario_dark.toml"))
        (cons "dracula_at_night"
              (file-append helix-source
                           "/runtime/themes/dracula_at_night.toml"))
        (cons "dracula"
              (file-append helix-source "/runtime/themes/dracula.toml"))
        (cons "emacs"
              (file-append helix-source "/runtime/themes/emacs.toml"))
        (cons "everblush"
              (file-append helix-source "/runtime/themes/everblush.toml"))
        (cons "everforest_dark"
              (file-append helix-source "/runtime/themes/everforest_dark.toml"))
        (cons "everforest_light"
              (file-append helix-source
                           "/runtime/themes/everforest_light.toml"))
        (cons "ferra"
              (file-append helix-source "/runtime/themes/ferra.toml"))
        (cons "flatwhite"
              (file-append helix-source "/runtime/themes/flatwhite.toml"))
        (cons "fleet_dark"
              (file-append helix-source "/runtime/themes/fleet_dark.toml"))
        (cons "github_dark_colorblind"
              (file-append helix-source
                           "/runtime/themes/github_dark_colorblind.toml"))
        (cons "github_dark_dimmed"
              (file-append helix-source
                           "/runtime/themes/github_dark_dimmed.toml"))
        (cons "github_dark_high_contrast"
              (file-append helix-source
                           "/runtime/themes/github_dark_high_contrast.toml"))
        (cons "github_dark"
              (file-append helix-source "/runtime/themes/github_dark.toml"))
        (cons "github_dark_tritanopia"
              (file-append helix-source
                           "/runtime/themes/github_dark_tritanopia.toml"))
        (cons "github_light_colorblind"
              (file-append helix-source
                           "/runtime/themes/github_light_colorblind.toml"))
        (cons "github_light_high_contrast"
              (file-append helix-source
                           "/runtime/themes/github_light_high_contrast.toml"))
        (cons "github_light"
              (file-append helix-source "/runtime/themes/github_light.toml"))
        (cons "github_light_tritanopia"
              (file-append helix-source
                           "/runtime/themes/github_light_tritanopia.toml"))
        (cons "gruvbox_dark_hard"
              (file-append helix-source
                           "/runtime/themes/gruvbox_dark_hard.toml"))
        (cons "gruvbox_dark_soft"
              (file-append helix-source
                           "/runtime/themes/gruvbox_dark_soft.toml"))
        (cons "gruvbox_light"
              (file-append helix-source "/runtime/themes/gruvbox_light.toml"))
        (cons "gruvbox"
              (file-append helix-source "/runtime/themes/gruvbox.toml"))
        (cons "heisenberg"
              (file-append helix-source "/runtime/themes/heisenberg.toml"))
        (cons "hex_lavender"
              (file-append helix-source "/runtime/themes/hex_lavender.toml"))
        (cons "hex_steel"
              (file-append helix-source "/runtime/themes/hex_steel.toml"))
        (cons "hex_toxic"
              (file-append helix-source "/runtime/themes/hex_toxic.toml"))
        (cons "ingrid"
              (file-append helix-source "/runtime/themes/ingrid.toml"))
        (cons "jellybeans"
              (file-append helix-source "/runtime/themes/jellybeans.toml"))
        (cons "kanagawa"
              (file-append helix-source "/runtime/themes/kanagawa.toml"))
        (cons "kaolin-dark"
              (file-append helix-source "/runtime/themes/kaolin-dark.toml"))
        (cons "kaolin-light"
              (file-append helix-source "/runtime/themes/kaolin-light.toml"))
        (cons "kaolin-valley-dark"
              (file-append helix-source
                           "/runtime/themes/kaolin-valley-dark.toml"))
        (cons "material_darker"
              (file-append helix-source "/runtime/themes/material_darker.toml"))
        (cons "material_deep_ocean"
              (file-append helix-source
                           "/runtime/themes/material_deep_ocean.toml"))
        (cons "material_oceanic"
              (file-append helix-source
                           "/runtime/themes/material_oceanic.toml"))
        (cons "material_palenight"
              (file-append helix-source
                           "/runtime/themes/material_palenight.toml"))
        (cons "meliora"
              (file-append helix-source "/runtime/themes/meliora.toml"))
        (cons "mellow"
              (file-append helix-source "/runtime/themes/mellow.toml"))
        (cons "merionette"
              (file-append helix-source "/runtime/themes/merionette.toml"))
        (cons "molokai"
              (file-append helix-source "/runtime/themes/molokai.toml"))
        (cons "monokai_aqua"
              (file-append helix-source "/runtime/themes/monokai_aqua.toml"))
        (cons "monokai_pro_machine"
              (file-append helix-source
                           "/runtime/themes/monokai_pro_machine.toml"))
        (cons "monokai_pro_octagon"
              (file-append helix-source
                           "/runtime/themes/monokai_pro_octagon.toml"))
        (cons "monokai_pro_ristretto"
              (file-append helix-source
                           "/runtime/themes/monokai_pro_ristretto.toml"))
        (cons "monokai_pro_spectrum"
              (file-append helix-source
                           "/runtime/themes/monokai_pro_spectrum.toml"))
        (cons "monokai_pro"
              (file-append helix-source "/runtime/themes/monokai_pro.toml"))
        (cons "monokai"
              (file-append helix-source "/runtime/themes/monokai.toml"))
        (cons "naysayer"
              (file-append helix-source "/runtime/themes/naysayer.toml"))
        (cons "new_moon"
              (file-append helix-source "/runtime/themes/new_moon.toml"))
        (cons "nightfox"
              (file-append helix-source "/runtime/themes/nightfox.toml"))
        (cons "night_owl"
              (file-append helix-source "/runtime/themes/night_owl.toml"))
        (cons "noctis_bordo"
              (file-append helix-source "/runtime/themes/noctis_bordo.toml"))
        (cons "noctis"
              (file-append helix-source "/runtime/themes/noctis.toml"))
        (cons "nord_light"
              (file-append helix-source "/runtime/themes/nord_light.toml"))
        (cons "nord-night"
              (file-append helix-source "/runtime/themes/nord-night.toml"))
        (cons "nord"
              (file-append helix-source "/runtime/themes/nord.toml"))
        (cons "onedarker"
              (file-append helix-source "/runtime/themes/onedarker.toml"))
        (cons "onedark"
              (file-append helix-source "/runtime/themes/onedark.toml"))
        (cons "onelight"
              (file-append helix-source "/runtime/themes/onelight.toml"))
        (cons "papercolor-dark"
              (file-append helix-source "/runtime/themes/papercolor-dark.toml"))
        (cons "papercolor-light"
              (file-append helix-source
                           "/runtime/themes/papercolor-light.toml"))
        (cons "penumbra+"
              (file-append helix-source "/runtime/themes/penumbra+.toml"))
        (cons "pop-dark"
              (file-append helix-source "/runtime/themes/pop-dark.toml"))
        (cons "rasmus"
              (file-append helix-source "/runtime/themes/rasmus.toml"))
        (cons "READ"
              (file-append helix-source "/runtime/themes/README.md"))
        (cons "rose_pine_dawn"
              (file-append helix-source "/runtime/themes/rose_pine_dawn.toml"))
        (cons "rose_pine_moon"
              (file-append helix-source "/runtime/themes/rose_pine_moon.toml"))
        (cons "rose_pine"
              (file-append helix-source "/runtime/themes/rose_pine.toml"))
        (cons "serika-dark"
              (file-append helix-source "/runtime/themes/serika-dark.toml"))
        (cons "serika-light"
              (file-append helix-source "/runtime/themes/serika-light.toml"))
        (cons "snazzy"
              (file-append helix-source "/runtime/themes/snazzy.toml"))
        (cons "solarized_dark"
              (file-append helix-source "/runtime/themes/solarized_dark.toml"))
        (cons "solarized_light"
              (file-append helix-source "/runtime/themes/solarized_light.toml"))
        (cons "sonokai"
              (file-append helix-source "/runtime/themes/sonokai.toml"))
        (cons "spacebones_light"
              (file-append helix-source
                           "/runtime/themes/spacebones_light.toml"))
        (cons "tokyonight_storm"
              (file-append helix-source
                           "/runtime/themes/tokyonight_storm.toml"))
        (cons "tokyonight"
              (file-append helix-source "/runtime/themes/tokyonight.toml"))
        (cons "varua"
              (file-append helix-source "/runtime/themes/varua.toml"))
        (cons "vim_dark_high_contrast"
              (file-append helix-source
                           "/runtime/themes/vim_dark_high_contrast.toml"))
        (cons "yellowed"
              (file-append helix-source "/runtime/themes/yellowed.toml"))
        (cons "zed_onedark"
              (file-append helix-source "/runtime/themes/zed_onedark.toml"))
        (cons "zed_onelight"
              (file-append helix-source "/runtime/themes/zed_onelight.toml"))
        (cons "zenburn"
              (file-append helix-source "/runtime/themes/zenburn.toml"))))

(define-record-type* <helix-configuration> helix-configuration
                     make-helix-configuration
  helix-configuration? this-helix-configuration
  ;; a-list: ( language name)-> ( ( grammar package) | ( grammar shared object library))
  (grammars helix-configuration-grammars
            (default %all-guix-tree-sitter-grammars))
  ;; a-list: ( language name)-> ( query directory)
  (queries helix-configuration-queries
           (default %shipped-helix-query-folders))
  ;; a-list: ( theme name)-> ( theme toml)
  (themes helix-configuration-themes
          (default %shipped-helix-themes)))

(define (squash-alist alist)
  (map (lambda (pair)
         (let ((index (car pair)))
           (list index
                 (assoc-ref alist index)))) alist))

; TOOD, handle system service configuration, perhaps by configuring the search path built into the helix binary
(define home-helix-service-type
  (service-type (name 'helix)
                (extensions (list (service-extension
                                   home-xdg-configuration-files-service-type
                                   (lambda (config)
                                     `(("helix/runtime" ,(file-union "runtime"
                                                          `(( ;made awkward by how the name for the .so is obtained during the build from grammar.js
                                                             ;; instead for now, search each package /lib/tree-sitter folder for the expected singular shared object
                                                             ;; default to first entry
                                                             ;; a future alternative is to compile each grammar halfway before the needed output is deleted
                                                             "grammars"
                                                             ,(receive (tree-sitter-packages
                                                                        library-files)
                                                                       (partition (lambda 
                                                                                          (grammar-entry-pair)
                                                                                    (let 
                                                                                         (
                                                                                          (grammar-entry
                                                                                           (cdr
                                                                                            grammar-entry-pair)))
                                                                                      (and
                                                                                       (package?
                                                                                        grammar-entry)
                                                                                       (equal?
                                                                                        (build-system-name
                                                                                         (package-build-system
                                                                                          grammar-entry))
                                                                                        'tree-sitter))))
                                                                        (helix-configuration-grammars
                                                                         config))
                                                                       (directory-union
                                                                        "helix-grammar-union"
                                                                        (list (file-union
                                                                               "helix-grammars"
                                                                               (squash-alist
                                                                                library-files))
                                                                              (computed-file
                                                                               "found-grammar-files"
                                                                               #~(begin
                                                                                   
                                                                                   (use-modules
                                                                                    (ice-9
                                                                                     ftw))
                                                                                   (mkdir #$output)
                                                                                   ;; have to seperate and recombine string list
                                                                                   
                                                                                   (for-each (lambda 
                                                                                                     (language-name
                                                                                                      language-package)
                                                                                               (let* 
                                                                                                     (
                                                                                                      (lib-dir
                                                                                                       (string-append
                                                                                                        language-package
                                                                                                        "/lib/tree-sitter"))
                                                                                                      
                                                                                                      (library-file
                                                                                                       (string-append
                                                                                                        lib-dir
                                                                                                        "/"
                                                                                                        (list-ref
                                                                                                         (scandir
                                                                                                          lib-dir
                                                                                                          (lambda 
                                                                                                                  (name)
                                                                                                            
                                                                                                            (not
                                                                                                             (member
                                                                                                              name
                                                                                                              (list
                                                                                                               "."
                                                                                                               "..")))))
                                                                                                         0))))
                                                                                                 
                                                                                                 (symlink
                                                                                                  library-file
                                                                                                  (string-append #$output
                                                                                                   "/"
                                                                                                   language-name
                                                                                                   ".so"))))
                                                                                    (list #$@
                                                                                     (map (lambda 
                                                                                                  (q)
                                                                                            
                                                                                            (car
                                                                                             q))
                                                                                      tree-sitter-packages))
                                                                                    (list #$@
                                                                                     (map (lambda 
                                                                                                  (q)
                                                                                            
                                                                                            (cdr
                                                                                             q))
                                                                                      tree-sitter-packages)))))))))
                                                            ("queries" ,(file-union
                                                                         "helix-queries"
                                                                         (squash-alist
                                                                          (helix-configuration-queries
                                                                           config))))
                                                            ("themes" ,(file-union
                                                                        "helix-themes"
                                                                        (squash-alist
                                                                         (helix-configuration-themes
                                                                          config))))
                                                            ("tutor" ,(file-append
                                                                       helix-source
                                                                       "/runtime/tutor"))))))))))
                (description
                 "Creation of the helix runtime directory for grammars, queries, themes, tutor")))
