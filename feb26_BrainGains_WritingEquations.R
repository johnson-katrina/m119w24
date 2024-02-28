#Define Functions
area_pen <- function(x){1200*x - x^2}
area_pen_next_to_barn <- function(y){2400*y -2*y^2}
volume_box_cost96 <- function(s){24*s-1/2*s^3}
volume_box_material6 <- function(x){3/2*x - 1/4*x^3}

#Select Input Values
input_pens <- seq(0,900,0.1)
input_boxes <- seq(0,5,0.001)

#Calculate Output Values
areas_pen <- area_pen(input_pens)
areas_pen_next_to_barn <- area_pen_next_to_barn(input_pens)
volumes_96dollar_box <- volume_box_cost96(input_boxes)
volumes_box <- volume_box_material6(input_boxes)

#Plot Function and Visually Identify Maxima
plot(input_pens,areas_pen,type='l')
plot(input_pens,areas_pen_next_to_barn,type='l')
plot(input_boxes,volumes_96dollar_box,type='l')
plot(input_boxes,volumes_box,type="l")

plot(input_boxes,volumes_box,type="l",ylim=c(-1,2))

#Plot Function with Maximizers Identified
plot(input_pens,areas_pen,type='l')
abline(v=600,col="red")
plot(input_pens,areas_pen_next_to_barn,type='l')
abline(v=600,col=2)
plot(input_boxes,volumes_96dollar_box,type='l')
abline(v=4,col=2)
plot(input_boxes,volumes_box,type="l",ylim=c(-1,2))
abline(v=sqrt(2),col=2)

#Calculate the Maximum Values
area_pen(600)
area_pen_next_to_barn(600)
volume_box_cost96(4)
volume_box_material6(sqrt(2))