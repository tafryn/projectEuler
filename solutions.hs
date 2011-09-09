import Data.List(tails, nub, group, delete, foldl', intersect, inits, permutations, groupBy, isInfixOf, (\\), sortBy, elemIndex)
import Data.Function (on)
import Numeric
import Data.Time.Calendar(fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate(toWeekDate)
import Char(digitToInt,ord,toUpper,intToDigit,chr)
import System.IO(readFile,FilePath)
import List(sort, maximumBy, minimumBy)
import Data.Ratio
import Data.Bits(xor)
import Maybe
import Debug.Trace

isPrime 2 = True
isPrime n = length (trialDiv n) == 1


trialDiv n = filter (\x -> n `mod` x == 0) (2:[3,5..(n `div` 2)])

isPrime' n
    | n < 2 = False
    | otherwise = ipHelp n ((n+1) `div` 2) 2
    where ipHelp n m i | i > m = True
          ipHelp n m i = rem n i /= 0 && ipHelp n m (i+1)

ldf k n | rem n k == 0 = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

factors n | n < 1 = error "Negative"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ldf 2 n

divisors n | n < 1 = error "Negative"
           | otherwise = divisors' n ((n+1) `div` 2)
    where divisors' n 0 = []
          divisors' n m | rem n m == 0 = m : divisors' n (m-1)
                        | otherwise = divisors' n (m-1)

{-fermatFactor :: (Floating a, RealFrac a) => a -> [a]-}
fermatFactor 1 = []
fermatFactor n | even n    =   2 : fermatFactor (div n 2)
               | otherwise = fac : fermatFactor (div n fac)
    where fac = (nextN (ceiling(sqrt (fromIntegral n))) n)

{-nextN :: (Integral a) => a -> a -> a-}
nextN a n | isSquare $ a^2 - n = a + b
          | otherwise = nextN (a+1) n
    where
        b = floor . sqrt . fromIntegral $ (a^2 -n)

isSquare n | n < 0 = False
           | otherwise = resquare n == n
    where resquare = (^2) . fromIntegral . floor . sqrt . fromIntegral

palindromic n = left == reverse right
    where nString = show n
          nLength = length nString
          left = take (nLength `div` 2) nString
          right = drop ((nLength+1) `div` 2) nString

twentyMod n = twentyMod' n 20
    where twentyMod' n 1 = True
          twentyMod' n d = rem n d == 0 && twentyMod' n (d-1)

buildPrimes = 2:(buildPrimes' [2] 3)
buildPrimes' lst n = if (length . filter (\x -> rem n x == 0)) lst == 0
                         then n:(buildPrimes' (n:lst) (n+2))
                         else buildPrimes' lst (n+2)

isPrime'' x
    | x < 2 = False
    | otherwise = isPrimeHelper x primesMine
    where
        isPrimeHelper n (p:ps)
            | p*p > n = True
            | n `mod` p == 0 = False
            | otherwise = isPrimeHelper n ps

primesMine = 2 : filter isPrime'' [3..]

number = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

fiveConsecutiveProduct n = helper . show $ n
    where helper (x:xs) | length (x:xs) >= 4 = max (prodFunc . take 5 $ (x:xs)) (helper xs)
                        | otherwise = 0
          prodFunc [] = 1
          prodFunc (x:xs) = read (x:[]) * prodFunc xs

thousandTriplets = [ (x,y,z) | x <- [1..999], y <- [1..999], z <- [1..999], x + y + z == 1000]

pythagoreanTriplets' = [ (x,y,z) | x <- [1..999], y <- [1..999], z <- [1000 - x - y], x^2 + y^2 == z^2]

pythagoreanTriplets = filter (\(x,y,z) -> x*x + y*y == z*z) thousandTriplets

sumPrimesBelow n = sum . takeWhile (<n) $ primes

row0  = [08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08]
row1  = [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00]
row2  = [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65]
row3  = [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91]
row4  = [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80]
row5  = [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50]
row6  = [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70]
row7  = [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21]
row8  = [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72]
row9  = [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95]
row10 = [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92]
row11 = [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57]
row12 = [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58]
row13 = [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40]
row14 = [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66]
row15 = [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69]
row16 = [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36]
row17 = [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16]
row18 = [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54]
row19 = [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]

grid = [row0,row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,row16,row17,row18,row19]

vmult (x,y) | x+3 < length grid = product [ grid !! x' !! y' | x' <- [x..x+3], y' <- [y]]
            | otherwise = error ("Index out of bounds" ++ (show x) ++ (show y))

hmult (x,y) | y+3 < length (grid !! x) = product [ grid !! x' !! y' | x' <- [x], y' <- [y..y+3]]
            | otherwise = error ("Index out of bounds" ++ (show x) ++ (show y))

dmult (x,y) | y+3 < length (grid !! x) && x+3 < length grid =
    product [ grid !! x' !! y' | (x',y') <- zip [x..x+3] [y..y+3]]

drmult (x,y) | y+3 < length (grid !! x) && x-3 >= 0 =
    product [ grid !! x' !! y' | (x',y') <- zip [x,x-1..x-3] [y..y+3]]

maxvmult = maximum [ vmult(x,y) | x <- [0..(length grid - 4)],
                                  y <- [0..(length (grid !! x) - 1)]]

maxhmult = maximum [ hmult(x,y) | x <- [0..length grid - 1],
                                  y <- [0..length (grid !! x) - 4]]

maxdmult = maximum [ dmult(x,y) | x <- [0..length grid - 4],
                                  y <- [0..length (grid !! x) - 4]]

maxdrmult = maximum [ drmult(x,y) | x <- [3..length grid - 1],
                                    y <- [0..length (grid !! x) - 4]]

triangle' n = sum [1..n]

triangle n = (n^2 + n) `div` 2

combinations 0 _ = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs,
                             ys <- combinations (n-1) xs']

divisors' n = map product . nub . ([1]:) . flatten . map (flip combinations facs) $ [1..(length facs - 1)]
    where facs = factors n
          flatten = foldl (++) []

countDivisors = product . map ((+1) . length) . group . factors

-- Something to do with combinations and limited multisets.
-- Combinations of multisets with finite multiplicities
-- Percy MacMahon
over500 = head $ filter ((>500) . countDivisors) $ map triangle' [1..]

fiftyDigitNumbers = [
    37107287533902102798797998220837590246510135740250,
    46376937677490009712648124896970078050417018260538,
    74324986199524741059474233309513058123726617309629,
    91942213363574161572522430563301811072406154908250,
    23067588207539346171171980310421047513778063246676,
    89261670696623633820136378418383684178734361726757,
    28112879812849979408065481931592621691275889832738,
    44274228917432520321923589422876796487670272189318,
    47451445736001306439091167216856844588711603153276,
    70386486105843025439939619828917593665686757934951,
    62176457141856560629502157223196586755079324193331,
    64906352462741904929101432445813822663347944758178,
    92575867718337217661963751590579239728245598838407,
    58203565325359399008402633568948830189458628227828,
    80181199384826282014278194139940567587151170094390,
    35398664372827112653829987240784473053190104293586,
    86515506006295864861532075273371959191420517255829,
    71693888707715466499115593487603532921714970056938,
    54370070576826684624621495650076471787294438377604,
    53282654108756828443191190634694037855217779295145,
    36123272525000296071075082563815656710885258350721,
    45876576172410976447339110607218265236877223636045,
    17423706905851860660448207621209813287860733969412,
    81142660418086830619328460811191061556940512689692,
    51934325451728388641918047049293215058642563049483,
    62467221648435076201727918039944693004732956340691,
    15732444386908125794514089057706229429197107928209,
    55037687525678773091862540744969844508330393682126,
    18336384825330154686196124348767681297534375946515,
    80386287592878490201521685554828717201219257766954,
    78182833757993103614740356856449095527097864797581,
    16726320100436897842553539920931837441497806860984,
    48403098129077791799088218795327364475675590848030,
    87086987551392711854517078544161852424320693150332,
    59959406895756536782107074926966537676326235447210,
    69793950679652694742597709739166693763042633987085,
    41052684708299085211399427365734116182760315001271,
    65378607361501080857009149939512557028198746004375,
    35829035317434717326932123578154982629742552737307,
    94953759765105305946966067683156574377167401875275,
    88902802571733229619176668713819931811048770190271,
    25267680276078003013678680992525463401061632866526,
    36270218540497705585629946580636237993140746255962,
    24074486908231174977792365466257246923322810917141,
    91430288197103288597806669760892938638285025333403,
    34413065578016127815921815005561868836468420090470,
    23053081172816430487623791969842487255036638784583,
    11487696932154902810424020138335124462181441773470,
    63783299490636259666498587618221225225512486764533,
    67720186971698544312419572409913959008952310058822,
    95548255300263520781532296796249481641953868218774,
    76085327132285723110424803456124867697064507995236,
    37774242535411291684276865538926205024910326572967,
    23701913275725675285653248258265463092207058596522,
    29798860272258331913126375147341994889534765745501,
    18495701454879288984856827726077713721403798879715,
    38298203783031473527721580348144513491373226651381,
    34829543829199918180278916522431027392251122869539,
    40957953066405232632538044100059654939159879593635,
    29746152185502371307642255121183693803580388584903,
    41698116222072977186158236678424689157993532961922,
    62467957194401269043877107275048102390895523597457,
    23189706772547915061505504953922979530901129967519,
    86188088225875314529584099251203829009407770775672,
    11306739708304724483816533873502340845647058077308,
    82959174767140363198008187129011875491310547126581,
    97623331044818386269515456334926366572897563400500,
    42846280183517070527831839425882145521227251250327,
    55121603546981200581762165212827652751691296897789,
    32238195734329339946437501907836945765883352399886,
    75506164965184775180738168837861091527357929701337,
    62177842752192623401942399639168044983993173312731,
    32924185707147349566916674687634660915035914677504,
    99518671430235219628894890102423325116913619626622,
    73267460800591547471830798392868535206946944540724,
    76841822524674417161514036427982273348055556214818,
    97142617910342598647204516893989422179826088076852,
    87783646182799346313767754307809363333018982642090,
    10848802521674670883215120185883543223812876952786,
    71329612474782464538636993009049310363619763878039,
    62184073572399794223406235393808339651327408011116,
    66627891981488087797941876876144230030984490851411,
    60661826293682836764744779239180335110989069790714,
    85786944089552990653640447425576083659976645795096,
    66024396409905389607120198219976047599490197230297,
    64913982680032973156037120041377903785566085089252,
    16730939319872750275468906903707539413042652315011,
    94809377245048795150954100921645863754710598436791,
    78639167021187492431995700641917969777599028300699,
    15368713711936614952811305876380278410754449733078,
    40789923115535562561142322423255033685442488917353,
    44889911501440648020369068063960672322193204149535,
    41503128880339536053299340368006977710650566631954,
    81234880673210146739058568557934581403627822703280,
    82616570773948327592232845941706525094512325230608,
    22918802058777319719839450180888072429661980811197,
    77158542502016545090413245809786882778948721859617,
    72107838435069186155435662884062257473692284509516,
    20849603980134001723930671666823555245252804609722,
    53503534226472524250874054075591789781264330331690]

firstTenOfSum = take 10 . show . sum $ fiftyDigitNumbers

collatzSequence 1 = [1]
collatzSequence n | even n = n : collatzSequence (n `div` 2)
                  | odd n = n : collatzSequence (3*n + 1)

longestCS = head . foldr maxLength [] . map collatzSequence $ [1..1000000]
    where maxLength x y | length x > length y = x
                        | otherwise = y

longestCS' 1 a = a
longestCS' n a = if length newSequence > length a
                 then longestCS' (n-1) newSequence
                 else longestCS' (n-1) a
    where newSequence = collatzSequence n

gridRoute n = choose (2*n) (n)

fac n = product [1..n]

choose n k = fac n `div` (fac k * (fac (n - k)))

numToWord n = thousandString ++ hundredString ++ connector ++ rest
    where thousands = n `div` 1000
          hundreds = rem n 1000 `div` 100
          thousandString | thousands > 0 = digitToWord thousands ++ "thousand"
                         | otherwise = ""
          hundredString | hundreds > 0 = digitToWord hundreds ++ "hundred"
                        | otherwise = ""
          rest = tensToWord (rem n 100)
          connector | length rest * (length hundredString + (length thousandString)) > 0 = "and"
                    | otherwise = ""

digitToWord 0 = ""
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

tensToWord 10 = "ten"
tensToWord 11 = "eleven"
tensToWord 12 = "twelve"
tensToWord 13 = "thirteen"
tensToWord 14 = "fourteen"
tensToWord 15 = "fifteen"
tensToWord 16 = "sixteen"
tensToWord 17 = "seventeen"
tensToWord 18 = "eighteen"
tensToWord 19 = "nineteen"
tensToWord n = tensString ++ onesString
    where
        onesString = digitToWord $ (rem n 10 `div` 1)
        tensDigit = n `div` 10
        tensString = tens tensDigit
        tens 0 = ""
        tens 2 = "twenty"
        tens 3 = "thirty"
        tens 4 = "forty"
        tens 5 = "fifty"
        tens 6 = "sixty"
        tens 7 = "seventy"
        tens 8 = "eighty"
        tens 9 = "ninety"

triangleData = [[75],
                [95,64],
                [17,47,82],
                [18,35,87,10],
                [20,04,82,47,65],
                [19,01,23,75,03,34],
                [88,02,77,73,07,63,67],
                [99,65,04,28,06,16,70,92],
                [41,41,26,56,83,40,80,70,33],
                [41,48,72,33,47,32,37,16,94,29],
                [53,71,44,65,25,43,91,52,97,51,14],
                [70,11,33,28,77,73,17,78,39,68,17,57],
                [91,71,52,38,17,14,91,43,58,50,27,29,48],
                [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
                [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

simpleWalk
    = map (\x -> triangleData !! (fst x) !! (snd x)) path
    where
        path = zip [0..] $ replicate (length triangleData) 0

greaterWalk (x,y) lst
    | x < 15    = lst !! x !! y : (greaterWalk (x+1, newY) lst)
    | otherwise = []
    where
        optionOne = lst !! (x+1) !! y
        optionTwo = lst !! (x+1) !! (y+1)
        newY = if optionOne > optionTwo
               then y
               else y+1

sumWalk (x,y) lst
    | x >= length lst = []
    | leftSum > rightSum = lst !! x !! y : (sumWalk (x+1, y) lst)
    | otherwise = lst !! x !! y : (sumWalk (x+1, y+1) lst)
    where
        subTriLeft = subTriangle (x+1, y) lst 14
        subTriRight = subTriangle (x+1, y+1) lst 14
        leftSum = sumLeft subTriLeft
        rightSum = sumLeft subTriRight

genericWalk path lst
    = map (\(x,y) -> lst !! x !! y) indexedPath
    where
        numPath = map (\(x,y) -> (fromInteger x, fromInteger y)) path
        indexedPath = indexHelper numPath 0
        indexHelper ((x,y):z) acc = (x,y+acc):(indexHelper z (acc+y))
        indexHelper [] _ = []

subTriangle (x,y) lst 0
    | x < length lst = [take 1 . snd . splitAt y $ lst !! x]
    | otherwise = []
subTriangle (x,y) lst depth
    | x+depth < length lst = rest ++ [currentLine]
    | otherwise = rest
    where
        currentLine = take (depth+1) . snd . splitAt y $ lst !! (x+depth)
        rest = subTriangle (x,y) lst (depth-1)

sumTriangle lst = foldr (\x y -> sum x + y) 0 lst

sumLeft lst = foldr (\x y -> head x + y) 0 lst

allPaths
    = map (zip [0..14]) values
    where
        values = [[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] | a <- [0],b <- [0,1],
                                                    c <- [0,1],d <- [0,1],
                                                    e <- [0,1],f <- [0,1],
                                                    g <- [0,1],h <- [0,1],
                                                    i <- [0,1],j <- [0,1],
                                                    k <- [0,1],l <- [0,1],
                                                    m <- [0,1],n <- [0,1],
                                                    o <- [0,1]]

countTriangle [xs] = xs
countTriangle (xs:xss) = let cs = countTriangle xss in zipWith (+) xs (zipWith max (init cs) (tail cs))

bestWalk triangle
    = foldr1 reduce triangle
    where
        reduce a b = zipWith (+) a (zipWith max b (tail b))

sundays
    = filter sundayTest dates
    where
        startDate = fromGregorian 1901 1 1
        endDate = fromGregorian 2000 12 31
        dates = [startDate..endDate]
        sundayTest x = let (y,m,d) = toGregorian x
                           (y',wn,wd) = toWeekDate x
                       in d == 1 && wd == 7

sumFacDigits
    = sum . map digitToInt . show . fac

sumOfAmicablePairs
    = sum . map fst $ amicablePairs
    where
        amicablePairs = filter (\(x,y) -> sumD y == x && x /= y) divisorSums
        divisorSums = [(x, sumD x) | x <- [1..9999]]
        sumD = sum . divisors

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiter string
    = chunk : splitBy delimiter rest'
    where
        chunk = takeWhile (/= delimiter) string
        rest = dropWhile (/= delimiter) $ string
        rest' | rest == [] = []
              | otherwise = tail rest

nameScore = do
    names <- readFile "names.txt"
    let nameList = sort . map (filter (/= '"')) . splitBy ','  $ names
        indexedNameList = zip [1..] nameList
        scoredList = map (\(x,y) -> x * (score y)) indexedNameList
    return (sum scoredList)
    where
        score s = sum . map letterScore $ s
        letterScore = (flip (-) 64) . ord . toUpper

abundantNumbers = filter (\x -> sum (divisors' x) > x) [12..]

notAbundantSum = filter (\x -> findSum x == []) [1..28123]

findSum n
    = findAnswer possibleFactors
    where
        findAnswer [] = []
        findAnswer (x:xs) | (n-x) `elem` (x:xs) = [(x,n-x)]
                          | otherwise = findAnswer xs
        possibleFactors = takeWhile (<n) abundantNumbers

toFacNumber n
    = foldr (:) [] . map intToDigit . reverse $ digits n 1
    where
        digits 0 i = []
        digits n i = rem n i : digits (n `div` i) (i+1)

permutation n lst
    = convert padFacNum lst
    where
        facNum = toFacNumber n
        padFacNum = replicate (length lst - (length facNum)) '0' ++ facNum
        convert [] _ = []
        convert (x:xs) lst = e : convert xs rest
            where
                (e, rest) = select (digitToInt x) lst

select i lst
    = (head back, front ++ (tail back))
    where
        (front,back) = splitAt i lst

fibs = 1:1:(zipWith (+) fibs (tail fibs))

fibLength n
    = (+1) . length . takeWhile (not . longEnough) $ fibs
    where
        longEnough = (==) n . length . show

longDiv x y
    | rem x y == 0 = [intToDigit (div x y)]
    | x < y = longDiv (x*10) y
    | otherwise = intToDigit (div x y) : longDiv (rem x y) y

longestRepeat lst
    = case result of
        [] -> 0
        _ -> minimum result
    where
        result = findRepeat lst 1
        next n = take n . drop n
        findRepeat lst x
            | x > 1000 = []
            | checkNext x = [x]
            | otherwise = findRepeat lst (x+1)
        checkNext n
            = (==1) . length . nub . take 10 $ chunkBy n lst
        chunkBy n lst
            = take n lst : chunkBy n (drop n lst)

largestRepeat = fst . maximumBy (\(_,x) (_,y) -> compare x y) $ zip [1..1000] (map (longestRepeat . longDiv 1) [1..1000])

coefficients = [(a,b) | a <- [(-999)..999], b <- [(-999)..999]]

quadratic (a,b)
    = \x -> x^2 + a*x + b

countProducedPrimes f
    = length . takeWhile isPrime'' . map f $ [0..]

temp = foldl' foldFunc ((0,0),0) $ zip coefficients (map (countProducedPrimes . quadratic) coefficients)

foldFunc (a,b) (x,y)
    | b > y = (a,b)
    | b < y = (x,y)
    | otherwise = (a,b)

mostSequentialPrimes = fst . foldl' foldFunc ((0,0),0) $ zip coefficients (map (countProducedPrimes . quadratic) coefficients)

diagonalSpiralSum depth
    = dSS' 2 (depth*2) 1
    where
        dSS' cur max n
            | cur >= max = 1
            | otherwise = sum [n+cur,n+cur*2..n+cur*4] + rest
            where
                rest = dSS' (cur+2) max (n+cur*4)

problem29 = length . nub $ [a^b | a <- [2..100], b <- [2..100]]

sumOfPowerDigits p n
    = n == digitSum
    where
        digitSum = sum . map (^p) . map digitToInt . show $ n

problem30 = sum $ filter (sumOfPowerDigits 5) [2..354294]

problem31
    = 1 + length [(a,b,c,d,e,f,g) |
        a <- [0..200],
        b <- [0..((200 - a) `div` 2)],
        c <- [0..((200 - a - 2*b) `div` 5)],
        d <- [0..((200 - a - 2*b - 5*c) `div` 10)],
        e <- [0..((200 - a - 2*b - 5*c - 10*d) `div` 20)],
        f <- [0..((200 - a - 2*b - 5*c - 10*d - 20*e) `div` 50)],
        g <- [0..((200 - a - 2*b - 5*c - 10*d - 20*e - 50*f) `div` 100)],
        1*a + 2*b + 5*c + 10*d + 20*e + 50*f + 100*g == 200]

foldFilter prefix a x
    | prefix x = x:a
    | otherwise = a

multiplications = [[a,b,a*b] | a <- [1..100], b <- [a..9876]]

problem32 = sum . nub . map (!!2) . foldl' (foldFilter prefixes) [] $ multiplications
    where
        prefixes x = (sort . convert) x == "123456789"
        convert = foldl (++) [] . map show

problem33 = filter equalByCancel allFracs
    where
        allFracs = [(b,a) | a <- [10..99], b <- [10..(a-1)]]
        equalByCancel (b,a) = normalDiv == abnormalDiv
            where
                commonDigit = intersect (show a) (show b)
                normalDiv = (fromInteger b) / (fromInteger a)
                abnormalDiv
                    | commonDigit /= [] && commonDigit /= ['0']= (fromIntegral . delCommonDigit $ b) /
                                          (fromIntegral . delCommonDigit $ a)
                    | otherwise = 0
                delCommonDigit n = head . map digitToInt . delete (head commonDigit) . show $ n

problem34 = sum . filter (\x -> x == digitFacSum x) $ [3..2540160]
    where
        digitFacSum = sum . map fac . map digitToInt . show

digitRotations n = map read $ zipWith (++) (tails . show $ n) (inits . show $ n) :: [Int]

problem35
    = length $ filter (all isPrime'' . digitRotations) [1..1000000]

isDoublePalindromic n
    = number == reverse number && binaryNumber == reverse binaryNumber
    where
        number = show n
        binaryNumber = showIntAtBase 2 intToDigit n ""

problem36
    = [x | x <- [1..999999], isDoublePalindromic x]

problem37
    = take 11 . filter isTruncPrime $ drop 4 primes
    where
        isTruncPrime p = (all isPrime'' . map read . delete "" . nub) $ (inits . show) p ++ ((tails . show) p)

problem38
    = maximumBy (\x y -> compare (concatProduct x) (concatProduct y)) pandigitalList
    where
        concatProduct (n,lst) = concatMap (show . (n*)) lst
        totalList = [zip (repeat a) (tail (inits [1..])) | a <- [1..9876]]
        limitedList = foldl' (++) [] $ map (takeWhile ((<=9) . length . concatProduct)) totalList
        pandigitalList = filter ((=="123456789") . sort . concatProduct) limitedList

pyTriples top
    = [[m^2 + n^2, 2*m*n, m^2 - n^2] |
        n <- [1..top],
        m <- [(n+1),(n+3)..top],
        gcd m n == 1]

pythagoreanTriplets'' n
    = [ [x,y,z] | x <- [1..n-2], y <- [1..(min (n/2) (x-1))], z <- [n - x - y], x^2 + y^2 == z^2]

problem39
    = snd . maximum . map (\x -> ((length . pythagoreanTriplets'') x,x)) $ [12,14..1000]

problem39'
    = head .
      maximumBy (\a b -> compare (length a) (length b)) .
      group .
      sort .
      concat .
      map (\s -> [s,(2*s)..999]) .
      map sum $
      pyTriples 50

takeByPowersUpTo n lst
    = helper 0 n lst
    where
        helper c m lst
            | c <= m = lst !! (10^c) : helper (c+1) m lst
            | otherwise = []

problem40
    = product . map digitToInt . takeByPowersUpTo 6 . concatMap show $ [0..]

isPandigital n
    = take (length . show $ n) "0123456789" == (sort . show $ n)

problem41
    = maximum . filter isPrime'' . map read . concatMap permutations . init . tails $ "987654321"

charToValue
    = (subtract 64) . ord . toUpper

problem42
    = do
    words <- readFile "words.txt"
    let wordList = sort . map (filter (/= '"')) . splitBy ',' $ words
    return (length .
            filter (`elem` (take 50 . map triangle $ [1..])) .
            map (sum . (map charToValue)) $
            wordList)

isPanPrimeDivisible n
    = all (\(x,y) -> rem x y == 0) primesAndChunks
    where
        primesAndChunks = zip chunks primes
        chunks = map (read . take 3) . take 7 . tail . tails . show $ n

problem43
    = sum .
      filter isPanPrimeDivisible .
      filter isPandigital .
      map read .
      permutations $
      "1234567890"

pentagonal n = n * (3*n - 1) `div` 2

problem44
    = head [ [x,y] | x <- pentagonals,
                     y <- takeWhile (<x) pentagonals,
                     x + y `elem` (takeWhile (<=(x+y)) pentagonals),
                     x - y `elem` (takeWhile (<=(x+y)) pentagonals)]
    where
        pentagonals = map pentagonal [1..]

hexagonal n = n * (2*n - 1)

problem45
    = merge pentagonals hexagonals
    where
        merge ps (h:hs)
            = let (rp:rps) = dropWhile (<h) ps
              in if h == rp
                 then h : merge rps hs
                 else merge rps hs
        triangles = map triangle [1..]
        pentagonals = map pentagonal [1..]
        hexagonals = map hexagonal [1..]

problem46
    = findFirstNonSum oddComposites
    where
        oddComposites = filter (not . isPrime'') [9,11..]
        squares = map (^2) [1..]
        findFirstNonSum (x:xs)
            | True `elem` answer = findFirstNonSum xs
            | otherwise = x
            where
                answerSpace = [ (p,s) | p <- takeWhile (<x) primesMine,
                                        s <- takeWhile (<(x-p)) squares]
                answer = map ((==x) . (\(p,s) -> p + 2*s)) answerSpace

problem47
    = firstOfFour 1
    where
        fourFactor n = (length . nub . factors) n == 4
        firstOfFour n
            | all fourFactor [n..n+3] = n
            | otherwise = firstOfFour (n+1)

problem48
    = reverse . take 10 . reverse . show . sum . take 1000 $ [x^x | x <- [1..]]

problem49
    = [ [x,x+y,x+2*y] | x <- [1000..9997],
                        y <- [1..(10000-x)`div`2],
                        all isPrime'' [x,x+y,x+2*y],
                        all (`elem` (permutations (show x))) (map show [x,x+y,x+2*y])]

problem50
    = maximumBy (\x y -> compare (length x) (length y)) .
      filter (isPrime'' . sum) .
      filter ((<1000000) . sum) $
      concatMap inits (tails ps)
    where
        ps = takeWhile (<4000) primesMine

indexReplace n lst d
    = read $ startReplace (show n) (intToDigit d) lst 0 :: Int
    where
        startReplace n d lst i
            | i >= length n = []
            | 0 `elem` lst && d == '0' = "0"
            | i `elem` lst = d : startReplace n d lst (i+1)
            | otherwise = n !! i : startReplace n d lst (i+1)

indicies x
    = concat [ combinations n m | n <- [1..(length . show) x],
                                  m <- [take (length . show $ x) [0..]]]

countTrues [] = 0
countTrues (x:xs)
    | x = 1 + countTrues xs
    | otherwise = countTrues xs

replacements x
    = [ map (indexReplace num iLst) [0..9] | iLst <- indicies x, num <- [x]]

-- This problem was very poorly worded.
problem51 (x:xs)
    | (>=8) . maximum . map (countTrues . (map isPrime'')) . replacements $ x = [x]
    | otherwise = problem51 xs

problem52 = head [ x | x <- [1..], sort (show x) == sort (show (2*x)),
                                   sort (show x) == sort (show (3*x)),
                                   sort (show x) == sort (show (4*x)),
                                   sort (show x) == sort (show (5*x)),
                                   sort (show x) == sort (show (6*x))]

problem53 = length [ choose x y | x <- [1..100], y <- [1..x], choose x y > 1000000]

data FaceValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
               | Jack | Queen | King | Ace
               deriving (Show,Ord,Eq,Enum)
data Suit = Clubs | Diamonds | Hearts | Spades
          deriving (Show, Ord, Eq)
data Card = Card FaceValue Suit
          deriving (Show, Ord, Eq)
data Rank = HighCard Card | Pair FaceValue | TwoPair FaceValue FaceValue
          | ThreeOfAKind FaceValue | Straight FaceValue FaceValue
          | Flush Card | FullHouse FaceValue FaceValue
          | FourOfAKind FaceValue | StraightFlush Card Card | RoyalFlush Suit
          deriving (Show, Ord, Eq)
type Hand = [Card]

suit (Card v s) = s

value (Card v s) = v

royalFlush hand
    | length hand == 5 = flush hand &&
                         straight hand &&
                         value (highCard hand) == Ace
    | otherwise = error "Incorrectly sized hand."

straightFlush hand
    | length hand == 5 = flush hand &&
                         straight hand
    | otherwise = error "Incorrectly sized hand."

fourOfAKind hand
    | length hand == 5 = (==4) . maximum . map length $ groupedHand
    | otherwise = error "Incorrectly sized hand."
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

fullHouse hand
    | length hand == 5 = elem 3 (map length groupedHand) &&
                         elem 2 (map length groupedHand)
    | otherwise = error "Incorrectly sized hand."
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

flush hand
    | length hand == 5 = (==1) . length . group . map suit $ hand
    | otherwise = error "Incorrectly sized hand."

straight hand
    | length hand == 5 = [value . lowCard $ hand..value . highCard $ hand] == sort (map value hand)
    | otherwise = error "Incorrectly sized hand."

threeOfAKind hand
    | length hand == 5 = elem 3 (map length groupedHand)
    | otherwise = error "Incorrectly sized hand."
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

twoPair hand
    | length hand == 5 = (==2) . length . filter (==2) $
                         (map length groupedHand)
    | otherwise = error "Incorrectly sized hand."
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

pair hand
    | length hand == 5 = elem 2 (map length groupedHand)
    | otherwise = error "Incorrectly sized hand."
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

highCard hand
    | length hand == 5 = maximum hand
    | otherwise = error "Incorrectly sized hand."

lowCard hand
    | length hand == 5 = minimum hand
    | otherwise = error "Incorrectly sized hand."


rank hand
    | royalFlush hand = RoyalFlush (suit . highCard $ hand)
    | straightFlush hand = StraightFlush (lowCard hand) (highCard hand)
    | fourOfAKind hand = FourOfAKind (value . head . head .
                                      filter ((==4) . length) $ groupedHand)
    | fullHouse hand = FullHouse (value . head . head .
                                  filter ((==3) . length) $ groupedHand)
                                 (value . head . head .
                                  filter ((==2) . length) $ groupedHand)
    | flush hand = Flush (highCard hand)
    | straight hand = Straight (value . highCard $ hand)
                               (value . lowCard $ hand)
    | threeOfAKind hand = ThreeOfAKind (value . head . head .
                                        filter ((==3) . length) $ groupedHand)
    | twoPair hand = TwoPair (head . tail . sort . map (value . head) .
                              filter ((==2) . length) $ groupedHand)
                             (head . sort . map (value . head) .
                              filter ((==2) . length) $ groupedHand)
    | pair hand = Pair (value . head . head . filter ((==2) . length) $
                        groupedHand)
    | otherwise = HighCard (highCard hand)
    where
        groupedHand = groupBy ((==) `on` value) . sort $ hand

abbrToValue '2' = Two
abbrToValue '3' = Three
abbrToValue '4' = Four
abbrToValue '5' = Five
abbrToValue '6' = Six
abbrToValue '7' = Seven
abbrToValue '8' = Eight
abbrToValue '9' = Nine
abbrToValue 'T' = Ten
abbrToValue 'J' = Jack
abbrToValue 'Q' = Queen
abbrToValue 'K' = King
abbrToValue 'A' = Ace

abbrToSuit 'C' = Clubs
abbrToSuit 'D' = Diamonds
abbrToSuit 'H' = Hearts
abbrToSuit 'S' = Spades

abbrToCard (v:s:[])
    = Card (abbrToValue v) (abbrToSuit s)

substitute _ _ [] = []
substitute old new (x:xs)
    | x == old = new : substitute old new xs
    | otherwise = x : substitute old new xs

problem54 = do
    contents <- readFile "poker.txt"
    let matches = map ((\x -> compare (rank . take 5 $ x) (rank . drop 5 $ x)) . map abbrToCard . splitBy ' ' . delete '\r') . lines $ contents
    return . length . filter (==GT) $ matches

isLychrel n
    = helper (n + (read . reverse . show $ n)) 1
    where
        isPalindromic n = (show n) == reverse (show n)
        helper n i
            | i >= 50 = True
            | isPalindromic n = False
            | otherwise = helper (n + (read . reverse . show $ n)) (i+1)

problem55
    = length . filter isLychrel $ [1..9999]

problem56
    = maximum $ map digitalSum [ a^b | a <- [0..100], b <- [0..100] ]
    where
        digitalSum n = sum . map digitToInt . show $ n

rootTwo n
    = 1 + (continuedFraction n 0)
    where
        continuedFraction n i
            | i == n = 0
            | otherwise = (denominator rest) % (numerator rest)
            where
                rest = 2 + (continuedFraction n (i+1))

problem57
    = length . filter (\x -> (length . show . numerator $ x) > (length . show . denominator $ x)) $ map rootTwo [1..1000]

diagonalSpiral
    = 1 : (dS' 2 1)
    where
        dS' cur i
            = i+cur : i+cur*2 : i+cur*3 : i+cur*4 : dS' (cur+2) (i+cur*4)

problem58
    = (findN 1 []) * 2 - 1
    where
        findN n acc
            | numPrimes / total < 0.1 = n
            | otherwise = findN (n+1) diagonals
            where
                diagonals = acc ++ (map isPrime'' . take 4 . drop ((n-1)*4+1) $ diagonalSpiral)
                total = fromIntegral . length $ diagonals
                numPrimes = fromIntegral . length . filter id $ diagonals

decode cipherText key
    = map chr . zipWith xor cipherText . map ord . concat . repeat $ key

process _ [] = []
process cipherText (k:ks)
    | " the " `isInfixOf` plainText = (plainText, k) : process cipherText ks
    | otherwise = process cipherText ks
    where
        plainText = decode cipherText k

problem59
    = do
    contents <- readFile "cipher1.txt"
    let cipherText = map read . splitBy ',' . delete '\r' . delete '\n' $
                     contents :: [Int]
    let keyspace = [ a:b:c:[] | a <- ['a'..'z'],
                                b <- ['a'..'z'],
                                c <- ['a'..'z']]
    return (sum . map ord . fst . head $ process cipherText keyspace)

allConcatsPrime ps
    = all concatPrime combos
    where
        combos = (combinations 2 ps) ++ (map reverse $ combinations 2 ps)
        concatPrime ps = isPrime'' . read . concat . map show $ ps

shortPrimes = delete 2 . delete 5 $ takeWhile (<10000) primesMine
cPPairs = [[a,b] | a <- shortPrimes, b <- shortPrimes, allConcatsPrime [a,b]]
pairGroups = groupBy ((==) `on` head) cPPairs
matches n = sort . nub . map (head . tail) . filter (elem n) $ cPPairs
matches' n = map (!!1) . head . filter ((==n) . head . head) $ pairGroups

nextConcatPrime lst (p:ps)
    | allConcatsPrime (p:lst) = (p:lst) : nextConcatPrime lst ps
    | otherwise = nextConcatPrime lst ps

cCP :: Int -> [[[Int]]] -> [[[Int]]]
cCP 0 xs = xs
cCP n xs
    = cCP (n-1) normalNext
    where
        next = [ [b:acc, foldl1 intersect (map matches' (b:acc))]
               | [acc,a] <- xs
               , b <- a
               , allConcatsPrime (b:acc) ]
        normalNext = nub . map (map sort) $ next

problem60
    = sum . head . head $ partialAnswer
    where
        gtz = filter (\x -> (>0) . length . snd $ x)
        partialAnswer = cCP 5 [[[], shortPrimes]]

problem60' = do
    a <- takeWhile (<10000) primes
    let m = f a $ dropWhile (<= a) (takeWhile (<10000) primes)
    b <- m
    let n = f b $ dropWhile (<= b) m
    c <- n
    let o = f c $ dropWhile (<= c) n
    d <- o
    let p = f d $ dropWhile (<= d) o
    e <- p
    return [a,b,c,d,e]
    where
        f x = filter (\y -> allConcatsPrime (y:[x]))

heptagonal n = n * (5*n - 3) `div` 2

octagonal n = n * (3*n -2)

chain m n
    = (take 2 . show $ n) == (drop 2 . show $ m)

cyclic lst
    = check (last lst:lst)
    where
        check (x:y:xs) = chain x y && (check xs)
        check _ = True

allSetsPresent [] = True
allSetsPresent (x:xs)
    | elem x (map octagonal [1..58]) = allSetsPresent xs
    | elem x (map heptagonal [1..63]) = allSetsPresent xs
    | elem x (map hexagonal [1..70]) = allSetsPresent xs
    | elem x (map pentagonal [1..81]) = allSetsPresent xs
    | elem x (map (^2) [1..99]) = allSetsPresent xs
    | elem x (map triangle [1..140]) = allSetsPresent xs
    | otherwise = False

cyclicSet (a:b:c:d:e:f:[])
    = [ [u,v,w,x,y,z]
      | u <- a
      , v <- filter (chain u) b
      , w <- filter (chain v) c
      , x <- filter (chain w) d
      , y <- filter (chain x) e
      , z <- filter (chain y) f
      , chain z u
      ]

problem61
    = sum . head . head . filter (/=[]) . map cyclicSet . permutations
      $ [triangles,squares,pentagonals,hexagonals,heptagonals,octagonals]
    where
        triangles = map triangle [45..140]
        squares = map (^2) [32..99]
        pentagonals = map pentagonal [26..81]
        hexagonals = map hexagonal [23..70]
        heptagonals = map heptagonal [21..63]
        octagonals = map octagonal [19..58]

sortedElem e (x:xs)
    | e == x = True
    | e < x = False
    | otherwise = sortedElem e xs

cubes = map (^3) [1..]

cubesBetween x y
    = takeWhile (<y) . dropWhile (<x) $ cubes

cubePermutations n
    = length . filter (==(sort . show $ n)) $ cubeStrings
    where
        size = length . show $ n
        possibleCubes = cubesBetween (10^(size-1)) (10^size)
        cubeStrings = map (sort . show) possibleCubes

problem62
    = head . filter (\x -> cubePermutations x == 5) $ cubes

nDigitNthPower n
    = sortedElem n $ map (^(length . show $ n)) [1..]

problem63
    = helper 1 0
    where
        helper n acc
            | nLength == 0 = acc
            | otherwise = helper (n + 1) (acc + nLength)
            where
                nLength = length nextBatch
                nextBatch = takeWhile (< (10^n)) .
                            dropWhile (< (10^(n-1))) $
                            map (^n) [1..]

continuedFraction n
    = cfNext 0 1 a0
    where
        a0 = floor . sqrt . fromInteger $ n
        cfNext m d a
            = a : cfNext newM newD newA 
            where
                newM = d*a - m
                newD = floor $ fromInteger (n - (newM^2)) / (fromInteger d)
                newA = floor (fromInteger (a0 + newM) / (fromInteger newD))

computePeriod lst
    = helper 1 lst
    where
        helper n lst
            | n >= 1000 = Nothing
            | take 500 period == take 500 lst = Just n
            | otherwise = helper (n+1) lst
            where
                period = cycle . take n $ lst

cfPeriod n
    = length (takeWhile (/=firstCF) . tail . iterate nextCF $ firstCF) + 1
    where
        a0      = truncate . sqrt . fromIntegral $ n
        firstCF = nextCF (0, 1, a0)
        nextCF (m, d, a)
            = (m', d', a') 
            where
                m' = d*a - m
                d' = floor $ fromInteger (n - (m'^2)) / (fromInteger d)
                a' = floor (fromInteger (a0 + m') / (fromInteger d'))

problem64
    = length . filter odd . 
      map (fromJust . computePeriod . tail . continuedFraction) $
      takeWhile (<=10000) nonSquares
    where
        nonSquares = [floor x | x <- [2..10000] \\ (map (^2) [2..100])]

problem64'
    = length . filter odd . map cfPeriod $ nonSquares
    where
        nonSquares = [floor x | x <- [2..10000] \\ (map (^2) [2..100])]

cfEstimate (x:xs)
    | xs /= []  = fromIntegral x + (1 / (cfEstimate xs))
    | otherwise = fromIntegral x

{-cfEstimate' (x:xs)-}
    {-| xs /= [] = (x * lcm0 + (fst rest), snd rest)-}
    {-| otherwise = (1, x)-}
    {-where-}
        {-rest = cfEstimate' xs-}
        {-lcm0 = lcm x (snd rest)-}

cfFraction n
    = helper 1 n
    where
        helper i n
            | floor (i*n) == ceiling (i*n) = (floor (i*n),floor i)
            | otherwise = helper (i+1) n

eproximation
    = 2 : (concat [ [1,2*x,1] | x <- [1..]])

{-cfH :: Int -> [Int] -> Int-}
{-cfH n lst-}
    {-| n >= length lst = error-}
    {-| otherwise = hs n-}
    {-where-}
        {-hs 0 = head lst-}
        {-hs 1 = (head . tail $ lst) * (hs 0) + 1-}
        {-hs i = (lst !! i) * (hs (i-1)) + (hs (i-2))-}

hs 0 lst = head lst
hs 1 lst = (head . tail $ lst) * (hs 0 lst) + 1
hs i lst = (lst !! i) * (hs (i-1) lst) + (hs (i-2) lst)

hs' (x:xs) = 0:1:zipWith (\a b -> x * b + a) (hs' xs) (tail (hs' xs))

fibs' = 0: scanl (+) 1 fibs'

scan2l :: (a -> a -> b -> a) -> a -> a -> [b] -> [a]
scan2l f q0 q1 [] = [q0, q1]
scan2l f q0 q1 (x:xs)
    = q0 : scan2l f q1 (f q0 q1 x) xs

cfNumerator lst = scan2l (\a b c -> c*b + a) (head lst) ((head . tail $ lst) * (head lst) + 1) (tail . tail $ lst)

cfDenominator lst = scan2l (\a b c -> c*b + a) (1) (head . tail $ lst) (tail . tail $ lst)

hs''' lst = Data.Ratio.numerator . foldr1 (\x y -> x + 1/y) . map fromInteger . take 100 $ lst

problem65
    = sum . map digitToInt . show . head . drop 99 . cfNumerator $ eproximation

ithCF i n
    = (ithNumerator, ithDenominator)
    where
        ithNumerator = head . drop (i-1) . cfNumerator . continuedFraction $ n
        ithDenominator = head . drop (i-1) . cfDenominator . continuedFraction $ n

pell n
    = helper 1 n
    where
        helper i n
            | x^2 - n*y^2 == 1 = x
            | otherwise = helper (i+1) n
            where 
                (x, y) = ithCF i n

pell' n
    | odd r = fst $ ithCF (r+1) n
    | otherwise = fst $ ithCF (2*r + 2) n 
    where
        r = cfPeriod n - 1

problem66
    = head . reverse . sortBy (compare `on` pell') 
      $ [2..1000] \\ (map (^2) [1..100])

readTriangleData :: String -> IO [[Int]]
readTriangleData name = do
    contents <- readFile name
    let triangleData = map (map read . words) . lines $ contents
    return triangleData

problem67 = do
    triangleData <- readTriangleData "triangle.txt"
    return . bestWalk $ triangleData

{-primes' = 2 : 3 : stepIt primes-}

{-stepIt ps = (trace ( show $ take 2 ps) revstep) . reverse $ ps-}
{-revstep ps@(ultimo:penultimo:_) = primesInRange (penultimo^2) (ultimo^2) ps-}

{-primesInRange start end ps = filter isPrime [start .. end]-}

pick5
    = [ ([a,b,c,d,e],[f,g,h,i,j]) | a <- reverse [1..9], b <- reverse [1..9]
                                  , c <- reverse [1..9], d <- reverse [1..9]
                                  , e <- reverse [1..9], f <- [10]
                                  , g <- reverse [1..9], h <- reverse [1..9]
                                  , i <- reverse [1..9], j <- reverse [1..9]
                                  , all (==(f + a + b))
                                    [g+b+c, h+c+d, i+d+e, j+e+a]]

magic5gon
    = [ ([a,b,c,d,e],[f,g,h,i,j]) 
      | a <- digits
      , b <- digits \\ (a:[])
      , c <- digits \\ (a:b:[])
      , d <- digits \\ (a:b:c:[])
      , e <- digits \\ (a:b:c:d:[])
      , f <- (10:digits) \\ (a:b:c:d:e:[])
      , g <- (10:digits) \\ (a:b:c:d:e:f:[])
      , h <- (10:digits) \\ (a:b:c:d:e:f:g:[])
      , i <- (10:digits) \\ (a:b:c:d:e:f:g:h:[])
      , j <- (10:digits) \\ (a:b:c:d:e:f:g:h:i:[])
      , all (==(f + a + b)) [g+b+c, h+c+d, i+d+e, j+e+a]]
    where
        digits = [1..9]

nGonDigits :: (Show a, Ord a) => ([a],[a]) -> Integer
nGonDigits (inner,outer)
    = read . concatMap show . concatMap subset $ [outerMin..outerMin+n-1]
    where
        subset i = (head . drop (i `mod` n) $ outer):
                   (head . drop (i `mod` n) $ inner):
                   (head . drop ((i+1) `mod` n) $ inner):[]
        n = length $ inner
        Just outerMin = elemIndex (minimum outer) outer

problem68
    = maximum . map nGonDigits $ magic5gon

phi n
    = numerator . ((n%1)*) . product . map (\x -> 1 - (1%x)) . nub . factors . numerator $ n%1

problem69
    = foldl' (\x y -> maximumBy (compare `on` fn) [x,y]) 1
    where
        list = map (\x -> x-1) . takeWhile (<1000) $ primes
        fn x = x % (phi x)

problem69'
    = last . takeWhile (<=1000000) . map product . inits $ primes

olMinus (x:xs) (y:ys) = case (compare x y) of
    LT -> x : olMinus xs (y:ys)
    EQ -> olMinus xs ys
    GT -> olMinus (x:xs) ys
olMinus xs _ = xs

olUnion (x:xs) (y:ys) = case (compare x y) of
    LT -> x : olUnion xs (y:ys)
    EQ -> x : olUnion xs ys
    GT -> y : olUnion (x:xs) ys
olUnion xs ys = xs ++ ys

primesTME = 2 : ([3,5..] `olMinus` join [[p*p,p*p+2*p..] | p <- primes'])
    where
        join ((x:xs):t) = x : olUnion xs (join t)
        primes' = 3 : ([5,7..] `olMinus`
                       join [[p*p,p*p+2*p..] | p <- primes'])

primes = primesTME

isPermutation
    = (==) `on` sort . show

problem70
    = helper 2 list
    where
        rPrimes = reverse . takeWhile (<4000) . dropWhile (<1000) $ primes
        list = reverse . sort $ [ x*y | x <- rPrimes, y <- dropWhile (<=x) rPrimes, x*y < 10000000]
        helper a [] = a
        helper a (x:xs)
            | x > 10^7 = a
            | x % (phi x) < a % (phi a) &&
              phi x `isPermutation` x = trace (show (x,mod x 3)) (helper x xs)
            | otherwise = helper a xs

problem70'
    = fst . pairs
    where
        pairs n1
            = minimumBy (compare `on` f) [ (m,pm) | a <- gena, b <- genb, let m = a*b, n>m, let pm=m-a-b+1, isPerm m pm]
            where
                n = fromInteger n1
                gena = dropWhile (flsqr n) $ takeWhile (flsqr (2*n)) primes
                genb = dropWhile (flsqr (n `div` 2)) $ takeWhile (flsqr n) primes
                isPerm a b = null $ show a \\ show b
                flsqr n x = x < (floor . sqrt . fromInteger) n
                f (x,px) = fromIntegral x / (fromIntegral px)

main = do
    print $ problem70
