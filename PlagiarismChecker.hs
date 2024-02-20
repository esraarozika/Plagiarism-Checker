import System.IO
import Control.Monad
import Data.List
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.Table
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Cairo

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle         := "Plagiarism Checker"
               ,containerBorderWidth := 20
               , windowDefaultWidth  := 330
               , windowDefaultHeight := 360 ]
    
    display <- entryNew
    set display [ entryEditable :=False
                , entryXalign   := 0
                , entryText     := "lesfvjnm "]
                
    button <- buttonNew
    set button [ buttonLabel := "Hello World" ]
    on button buttonActivated $ do
     putStrLn "A \"clicked\"-handler to say \"destroy\""
    
    window `on` deleteEvent $ do -- handler to run on window destruction
        liftIO mainQuit
        return False
    set window [ containerChild := button ]
    widgetShowAll window
    mainGUI
                
    file1 <- readFile "/Users/esraarozika/Master/Theory of programming language/esraa.txt"
    file2 <- readFile "/Users/esraarozika/Master/Theory of programming language/latifa.txt"
    putStrLn file1
    putStrLn file2
    putStrLn $ show (words (takeword (lowerCase file1)))
    let l1 = words (takeword (lowerCase file1))
    let l2 = words (takeword (lowerCase file2))
    print "reading files done!"
    print l1
    print l2
    let allWords = l1 `union` l2
    print allWords
    let intList1 = concat(getnum(finalCounter allWords l1))
    print intList1
    let intList2 = concat(getnum(finalCounter allWords l2))
    print intList2
    let sum1 = sum intList2 `div` sum intList1
    print sum1
    putStrLn $ show (similarityCousine intList1 intList2)
           
takeword :: String -> String
takeword "" = ""
takeword (x:xs) = x : takeword xs

lowerCase :: String -> String
lowerCase = map toLower



countOccurencesIn xs x = length . filter (== x) $ xs

finalCounter xs ys = map (countOccurencesIn ys) $ xs

dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum [k * v | (k, v) <- zip xs ys]

getnum :: [Int] -> [[Int]]
getnum [] = []
getnum (x:xs)= if x>1 then replicate x 1 : getnum xs
       else [x] : getnum xs
       
similarityCousine :: [Int] -> [Int] -> Float
similarityCousine xs ys = ((fromIntegral (dotProduct xs ys) :: Float) / ((fromIntegral (sum xs * sum ys) :: Float) ** 0.5))* 100





      
      
