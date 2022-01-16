


-- Return table from 2->N (for primal test)
def min_table(n:i64) : []i64 =
    take(n-2)(tabulate(n)(\i -> i + 2))

-- Test is number n:i64 is prime return True:False
def is_prime(n:i64) : bool =
    any(\i -> n%i==0)(min_table(n))

-- Create array of value prime size N
def prime_array(n: i64) : []i64 =
    filter(\i -> !is_prime(i))(min_table(n))

-- Make GCD with euclid algorithm (https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclidean_algorithm)
def euclid_pgcd((i:i64,j:i64)) : (i64,i64) =
    iterate_until(\(_,y) -> y==0)(\(x,y) -> (y,x%y))(i,j)
    -- loop (i,j) while j!=0 do
    --     (j,i%j)

-- Make PCM with euclid GCD (https://en.wikipedia.org/wiki/Greatest_common_divisor#Least_common_multiple)
def euclid_ppcm((i:i64,j:i64)) : i64 =
    let (x,_) = euclid_pgcd(i,j)
    in (i64.abs(i*j)/x)

-- find P & Q (https://www.geeksforgeeks.org/how-to-solve-rsa-algorithm-problems/)
def find_p_q(n:i64) : []i64 = 
    let a = prime_array(n)
    let tmp:[]i64 = map2(\a b -> if b then a else 0)(a)(map(\i -> any(\k -> k==n)(map(\j -> i*j)(a)) )(a))
    in filter(\i -> i!=0)(tmp)

-- How quickly can we reduce arrays?
--
-- ==
-- nobench input { 3i64 }
-- output { true }
-- input { 100i64 }
-- output { false }
-- compiled input { 17i64 }
-- output { true }
-- !is_prime(n)

def main (n:i64): []i64 = 
    find_p_q(n)