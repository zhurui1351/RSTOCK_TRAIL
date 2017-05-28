require(R6)
library(pryr)  
Person <- R6Class("Person",    # 定义一个R6类
                    public=list(
                        hello = function(){         # 定义公有方法hello
                            print(paste("Hello"))
                          }
                      )
                  )

u1<-Person$new()
otype(Person)
otype(u1)

Person <- R6Class("Person",
                    public=list(
                        name=NA,                           # 公有属性
                        initialize = function(name){       # 构建函数方法
                            self$name <- name
                          },
                        hello = function(){                # 公有方法
                            print(paste("Hello",self$name))
                          }
                      )
                  )
conan <- Person$new('Conan')  
conan$hello()
conan$name = 'test' 
conan$hello()



Person <- R6Class("Person",
                     public=list(                       # 公有成员
                         name=NA,
                         initialize = function(name,gender){
                             self$name <- name
                             private$gender<- gender        # 给私有属性赋值
                           },
                         hello = function(){
                             print(paste("Hello",self$name))
                             private$myGender()             # 调用私有方法
                           }
                       ),
                     private=list(                      # 私有成员
                         gender=NA,
                         myGender=function(){
                             print(paste(self$name,"is",private$gender))
                           }
                       )
                   )

conan <- Person$new('Conan','Male')
conan$hello()  
conan$name 
conan$gender
conan$myGender()


Worker <- R6Class("Worker",
                     inherit = Person,                # 继承，指向父类
                     public=list(
                         bye = function(){
                             print(paste("bye",self$name))
                           }
                       )
                   )