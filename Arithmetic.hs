-- A Virtual Machine (VM) for Arithmetic (specification)

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Eq,Show)

-- Rational numbers
data QQ =  QQ II PP
  deriving (Eq,Show)

------------------------
-- Arithmetic on the  VM
------------------------

----------------
-- NN Arithmetic
----------------

-- add natural numbers
addN :: NN -> NN -> NN
addN O n = n
addN (S n) m = S (addN n m)

-- multiply natural numbers
multN :: NN -> NN -> NN
multN O n = O
multN (S n) m = addN (multN n m) m

-- subtract natural numbers
subN :: NN -> NN -> NN
subN O n = O
subN n O = n
subN (S n) (S m) = subN n m

----------------
-- II Arithmetic
----------------

-- Addition: (a-b)+(c-d)=(a+c)-(b+d)
addI :: II -> II -> II
addI (II a b) (II c d) = II (addN a c) (addN b d)

-- Multiplication: (a-b)*(c-d)=(ac+bd)-(ad+bc)
multI :: II -> II -> II
multI (II a b) (II c d) = II (multN a c) (multN b d)

-- Subtraction: (a-b)-(c-d)=(a+d)-(b+c)
subtrI :: II -> II -> II
subtrI (II a b) (II c d) = II (subN a d) (subN b c)

-- Negation: -(a-b)=(b-a)
negI :: II -> II
negI (II a b) = (II b a)

----------------
-- QQ Arithmetic
----------------

-- addition of positive numbers
-- use recursion over PP
addP :: PP -> PP -> PP
addP I n = (T n)
addP (T n) m = T (addP n m)

-- multiply positive numbers
-- use recursion over PP
multP :: PP -> PP -> PP
multP I n = n
multP (T n) m = addP m (multP n m)

-- Subtract positive from natural number
subNP :: NN -> PP -> NN
subNP (S n) I = n
subNP (S n) (T m) = subNP n m

-- convert numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp I = II (S O) O
ii_pp (T a) = addI (II (S O) O) (ii_pp a)

-- Addition: (a/b)+(c/d)=(ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (addI (multI a (ii_pp d)) (multI (ii_pp b) c)) (multP b d)

-- Multiplication: (a/b)*(c/d)=(ac)/(bd)
multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ c d) = QQ (multI a c)(multP b d)

----------------
-- Normalisation
----------------

normalizeI :: II -> II
normalizeI (II n O) = (II n O)
normalizeI (II O m) = (II O m)
normalizeI (II (S n) (S m)) = normalizeI (II n m)

----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

-- Precondition: Inputs are non-negative
-- recursion on Int
nn_int :: Integer -> NN
nn_int 0 = O
nn_int n = addN (S O) (nn_int (n - 1))

-- recursion on NN
int_nn :: NN -> Integer
int_nn O = 0
int_nn (S n) = 1 + (int_nn n)

-- utilize Integer and convert to II
ii_int :: Integer -> II
ii_int 0 = II O O
ii_int n = II (nn_int(n)) O

-- utilize II and convert to Integer
-- for multiple II values inside
int_ii :: II -> Integer
int_ii (II O O) = 0
int_ii (II (n) m) = int_nn(n) - int_nn(m)

-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int 1 = I
pp_int n = addP I (pp_int (n - 1))

int_pp :: PP -> Integer
int_pp I = 1s
int_pp (T n) = 1 + int_pp (n)

pp_nn :: PP -> NN
pp_nn I = (S O)
pp_nn (T n)= S (pp_nn n)

nn_pp :: NN -> PN
nn_pp (S O)= I
nn_pp (S n)= T (nn_pp n)

float_qq :: QQ -> Float
float_qq (QQ n m) = fromIntegral (int_ii n) / fromIntegral (int_pp m)

------------------------------
-- Normalisation by Evaluation
------------------------------

nbv :: II -> II
nbv m = ii_int (int_ii m)

------------
-- Booleans
------------

-- Less Than for Positive Integers
-- use recursion over PP
lessthanPP :: PP -> PP -> Bool
lessthanPP a I = False
lessthanPP I a = True
lessthanPP (T a) (T b) = lessthanPP a b


-- Greater Than for Positive Integers
-- use recursion over PP
greaterthanPP :: PP -> PP -> Bool
greaterthanPP a I = True
greaterthanPP I a = False
greaterthanPP (T a) (T b) = greaterthanPP a b


-- using Booleans in other functions such as insert
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:(insert x ys)

-- using Booleans in other functions such as sort
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insert x (sort xs)


------------
-- Fractions
------------
-- Declaring Fraction data type
data Fraction = NN NN

-- add Fractions
addFraction :: Fraction -> Fraction -> Fraction
addFraction ((S a),(S b)) (x,y) | (S b) == y = (addN (S a) x, y)
                                | (S b) /= y = addFraction (multN (S a) y, (multN (S b) y)) (multN x b), (multN (S a) y))

-- subtract Fractions
subFraction :: Fraction -> Fraction -> Fraction
subFraction ((S a),(S b)) (x,y) | (S b) == y = (subN (S a) x, y)
                              | (S b) /= y = addFraction (multN (S a) y, (multN (S b) y)) (multN x b), (multN (S a) y))


-- multiplication of Fractions
multFrac :: Fraction -> Fraction -> Fraction
multFrac ((S n),(T m)) (x,y) = (multN (S n) x , multP (T m) y)


-- equality of Fractions
equalityFrac :: Fraction -> Fraction -> Bool
equalityFrac ((S n),(T m)) (x,y) = (mult (S n) (pp_nn y)) == (mult (pp_nn (T m)) x)

-- subtraction of natural numbers
subNN :: NN -> NN -> NN
subNN O n = O
subNN n O = n
subNN (S n) (S m) = subNN n m

-- greatest common divisor for natural numbers
gcdNN :: NN -> NN -> NN
gcdNN O O = O
gcdNN O n = n
gcdNN n m | less n m==False = gcdNN m (subNN n m)
          | less n m==True = gcdNN n (subNN m n)


-- divide a natural number by a positive integers
divPP :: NN -> PN -> NN
divPP n m = if n == (pp_nn m) then (S O)
        else if (less n (pp_nn m)) then O
                else (add (S O) (divPP (subNN n m) m))


-- simplify fractions
simplifyFrac :: Fraction -> Fraction
simplifyFrac ((S n),(T m)) = ((divPP (S n) (nn_pp(gcdNN (S n) (pp_nn (T m))))),
                                 (nn_pp (divPP (pp_nn(T m)) (nn_pp(gcdNN (S n) (pp_nn (T m)))))))

----------
-- Testing
----------

main = do
    -- Integers: (II i j) represents i-j, (II k l) represents k-l
    let i = 4
    let j = 2
    let k = 1
    let l = 3
    print $ int_ii (addI (II (nn_int i) (nn_int j)) (II (nn_int k) (nn_int l)))
    print $ int_ii (multI (II (nn_int i) (nn_int j)) (II (nn_int k) (nn_int l)))
    -- Fractions: (QQ i j) represents i/j, (QQ k l) represents k/l
    print $ float_qq (addQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    print $ float_qq (multQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    -- Normalisation (recursive definition)
    print $ normalizeI (II (nn_int i) (nn_int j))
    -- Normalisation (by evaluation)
    print $ nbv (II (nn_int i) (nn_int j))

    print $ float_qq (QQ (ii_int 5) (pp_int 9))

    -- 2 + 1 = 3
    -- print $ int_nn (addN (nn_int j) (nn_int k))

    -- 4 * 3 = 12
    -- print $ int_nn (multN (nn_int i) (nn_int l))

    -- 4 - 2 = 2
    -- print $ int_nn (subN (nn_int i) (nn_int j))

    -- print $ int_ii (ii_int 10)
    -- print $ int_pp (pp_int 10)

    -- 4 + 2 = 6
    -- print $ int_ii (addI (nn_int i) (nn_int k))

    -- 4 + 2 = 6
    -- print $ int_pp (addP (pp_int i) (pp_int j))
