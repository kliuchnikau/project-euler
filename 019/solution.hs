-- https://projecteuler.net/problem=19
type Date = (Int,Int,Int)

numOfDaysInMonth (dayNo,monthNo,yearNo)
  | (monthNo == 2) && isLeapYear = 29
  | (monthNo == 2) && not isLeapYear = 28
  | elem monthNo [4,6,9,11] = 30
  | otherwise = 31
  where
    isLeapYear = yearDivisibleBy4 && (notCenturyYear || centuryDivisibleBy400)
    yearDivisibleBy4 = yearNo `mod` 4 == 0
    notCenturyYear = not (yearNo `mod` 100 == 0)
    centuryDivisibleBy400 = yearNo `mod` 400 == 0

nextDayOfWeek :: String -> String
nextDayOfWeek "Unknown" = "Unknown"
nextDayOfWeek currentDayOfWeek = daysOfWeek !! ((currentDayIdx+1) `mod` 7)
  where
    currentDayIdx = snd $ head $ filter (\(day,idx) -> day == currentDayOfWeek) (daysOfWeek `zip` [0..6])
    daysOfWeek = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

nextDay :: (Date, String) -> (Date, String)
nextDay ((dayNo,monthNo,yearNo), currentDayOfWeek) = (nextDate daysInMonth, nextDayOfWeek currentDayOfWeek)
  where
    daysInMonth = numOfDaysInMonth (dayNo,monthNo,yearNo)
    nextDate daysInMonth | dayNo == daysInMonth = if monthNo == 12 then (1,1,yearNo+1) else (1,monthNo+1,yearNo)
                         | otherwise = (dayNo+1,monthNo,yearNo)

allDays :: [(Date, String)]
allDays = ((1,1,1900), "Monday") : [nextDay(day num) | num <- [0..]]
  where
    day dayNum = allDays !! dayNum

daysRange :: (Date,Date) -> [(Date, String)]
daysRange (startDay,endDay) = takeWhile (\(day,dayOfWeek) -> not $ day == dayAfterEnd) $ dropWhile (\(day,dayOfWeek) -> not $ day == startDay) allDays
  where
    dayAfterEnd = fst $ nextDay(endDay, "Unknown")

isSundayOnFirst :: (Date, String) -> Bool
isSundayOnFirst ((1,_,_), "Sunday") = True
isSundayOnFirst _ = False

countSundaysOnFirst :: (Date,Date) -> Int
countSundaysOnFirst (from,to) = length [ day | day <- daysRange(from, to), isSundayOnFirst day]

-- countSundaysOnFirst ((1,1,1901),(31,12,2000))
