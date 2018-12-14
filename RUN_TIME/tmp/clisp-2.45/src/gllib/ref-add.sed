/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ clisp / clisp /
  tb
  s/ $/ clisp /
  :b
  s/^/# Packages using this file:/
}
