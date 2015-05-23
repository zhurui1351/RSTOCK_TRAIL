require('shiny')
#查看示例代码的位置
system.file('examples',package='shiny')
#演示demo
runExample("01_hello")
runExample("03_reactivity")
runApp("C:/R/code/RSTOCK_TRAIL/packageStudy/shiny/app-1",display.mode = "showcase")
runApp("C:/R/code/RSTOCK_TRAIL/packageStudy/shiny/myapp4")
#产生html代码
h1("My title")
