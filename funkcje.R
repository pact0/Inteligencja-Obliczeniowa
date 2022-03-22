f1 <- function(x1)
{
    return (x1)
}

f2 <- function(x1, x2, x3)
{
    return (1 - (1/(4*pi**2))*(x1+pi)**2 + abs(x2 - 5*cos(x1))**(1/3) + abs(x3 - 5*sin(x1))**(1/3))
}