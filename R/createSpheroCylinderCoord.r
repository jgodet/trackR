# createSpheroCylinderCoord.r
# written by JuG
# January 10 2018


#' Do something
#' @author JuG
#' @description None
#' @param N umber of points
#' @param l length of the rod
#' @param r radius
#' @param initialPos Initial position
#' @param rot Rotation
#' @details None
#' @examples
#' coord <- createSpheroCylinderCoord(rot=10)
#' plot(coord,asp=1)
#'
#' @return
#' @export


createSpheroCylinderCoord<- function(N = 1000, l =2 , r = .3, initialPos = c(1,1,0), rot = 0){

    #N = 1000 #nombre de points
    #l = 2  # en µm
    #r = .3
    a = l/2 - r

    #position z
    z = runif(N,-a-r, a+r )

    #détermination de phi
    phi <- numeric()

    for (i in 1:N){
      if(abs(z[i])<a){
        phi[i] = pi/2
      }
      if( z[i] > a){
        phi[i] = acos((z[i]-a)/r)
      }
      if( z [i]< -a){
        phi[i] = acos((z[i]+a)/r)
      }
    }

    #détermination de phi
    theta <- runif(N, 0, 2*pi)

    #détermination de z

    # x

    x = r * cos(theta) * sin(phi)
    y = r * sin(theta) * sin(phi)


    #rotation
    yr = y * cos(rot * 2 * pi /360) + z * sin(rot * 2 * pi /360)
    zr = z * cos(rot * 2 * pi /360) - y * sin(rot * 2 * pi /360)

    coord <- as.data.frame(cbind(zr + initialPos[1],yr + initialPos[2],x + initialPos[3] + r, x,y,z,theta))
    return(coord)
}
