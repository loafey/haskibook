### blabla blabla
yooawdawo
* 2121
*1 231
*421 dwa

![Alt text](https://assets.digitalocean.com/articles/alligator/boo.svg "a title")

```
@=lerpPoint
#=[Point 5 3, Point 10 3]

data Point = Point Float Float

lerpPoint :: Point -> Point -> Float -> Point
lerpPoint (Point x1 y1) (Point x2 y2) f = Point x y
    where
        diffX = (x2 - x1) * f
        diffY = (y2 - y1) * f
        x = diffX + x1
        y = diffY + y1
```

### blabla blabla
yooawdawo
* 2121
*1 231
*421 dwa