-- Making a module from scratch

-- (give it the same name as the file for convenience)

module Geometry
( sphereVol
, sphereArea
, cubeVol
, cubeArea
, cuboidVol
, cuboidArea
) where

sphereVol :: Float -> Float
sphereVol radius = (4.0 / 3.0) * pi * (radius^3)

sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius^2)  
  
cubeVol :: Float -> Float  
cubeVol side = cuboidVol side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side 

cuboidVol :: Float -> Float -> Float -> Float  
cuboidVol a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b 

-- Boy, working with float-to-float functions sure is simple

-- we can now use import Geometry, as long as our program is in the same folder as the module

-- This chapter also has instructions for splitting this into sub-modules
-- (with module names like Geometry.Cube, in the Geometry folder)