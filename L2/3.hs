dist :: Double -> Double -> Double
dist x y 
  | x < 0 = dist (-x) (-y)
  | y < 0 && d >= 5 = d - 5  -- IV квадрант Вне круга
  | y >= 0 && y > -x+5 && m < 5 = (x + y - 5) / sqrt 2 -- I квадрант вне треугольника в полосе
  | y >= 0 && y > -x+5 && m >= 5 = min (sqrt ( ( (x-5)^2 + y^2 ) ) )  $ sqrt ( (y-5)^2 + x^2 )  -- I квадрант вне треугольника вне полосы
  | otherwise = 0 -- точка внутри фигуры
  where
    m =  abs (y - x)
    d = sqrt $ x^2 + y^2
 
 -{
 =
    if x*y >= 0 
       then if (x+y-5 > 0) || (x+y+5 < 0)
               then if (y-x-5 > 0) || (y-x+5 < 0)
                       then min (sqrt ( ( (abs x)-5)^2 + y^2 ) ) (sqrt ( ( (abs y)-5)^2 + x^2 ) )
                       else (abs x + abs y - 5) / (sqrt 2)
               else 0
       else if x^2 + y^2 > 25
            then sqrt ( (x-x1)^2 + (y-y1)^2 ) 
            else 0
            where
            x1 =    (abs x)*5 / (sqrt (x^2 + y^2) )
            y1 = (- (abs y)*5 / (sqrt (x^2 + y^2) ) )
}-