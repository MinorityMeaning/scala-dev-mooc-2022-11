val x = None//Option("ЧЕТО-ЧЕТО")
val y = Option(4)

println(x.toString)
println(x.zip(y))

import module1.opt.Option

val x = Option(4)

x.filter(_ == 4)