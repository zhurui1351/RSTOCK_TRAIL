setClass("Person",slots=list(name="character",age="numeric"))
father<-new("Person",name="F",age=44)
father1<-new("Person",name="T",age=45)

# 设置属性age的默认值20
setClass("Person",slots=list(name="character",age="numeric"),prototype = list(age = 20))
n1<-new("Person",name="n1",age=19)
n2<-initialize(n1,name="n2")
n2@name
# 定义泛型函数work，即接口
setGeneric("work",function(object) standardGeneric("work"))
# 定义work的现实，并指定参数类型为Person对象
setMethod("work", signature(object = "Person"), function(object) cat(object@name , "is working"),sealed=T )
work(father)
work(father1)

work = function(object){print('123')}
work(father)
