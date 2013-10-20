;; fixing various windows-related bullshit

(when (eq system-type 'windows-nt)
  (eval-after-load 'inf-ruby
    '(setq-default
      inf-ruby-implementations
      '(("ruby"     . "d:/progs/Ruby200/bin/irb --prompt default -r irb/completion")
        ("jruby"    . "jruby -S irb --prompt default -r irb/completion")
        ("rubinius" . "rbx -r irb/completion")
        ("yarv"     . "irb1.9 -r irb/completion")
        ("macruby"  . "macirb -r irb/completion")
        ("pry"      . "pry")))))
