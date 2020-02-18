# #classTrack.r
# # written by JuG
# # December 17 2017
#
# setClass(
#   Class = "Track",
#   representation = representation(
#     Id = "integer",
#     spotId = "numeric",
#     x = "numeric",
#     y= "numeric",
#     t = "numeric",
#     median = "numeric",
#     mean = "numeric",
#     diameter = "numeric",
#     nSpots = "numeric",
#     jump = "numeric"
#   )
# )
#
#
# setMethod(f = "jump",signature = "Track",definition = function(object){
#
# }
#
# )
# a <- c(new(Class="Track", Id=as.integer(1)),new(Class="Track", Id=as.integer(2)))
#
#
# a[[2]]@Id
# getSlots("Track")
