
-- Return table from 2->N (for primal test)
def min_table(n:i64) : []i64 =
    take(n-2)(tabulate(n)(\i -> i + 2))

-- Test is number n:i64 is prime return True:False
def is_prime(n:i64) : bool =
    any(\i -> n%i==0)(min_table(n))

-- Create array of value prime size N
def prime_array (n: i64) : []i64 =
    filter(\i -> !is_prime(i))(min_table(n))

-- Make GCD with euclid algorithm (https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclidean_algorithm)
def euclid_pgcd((i:i64,j:i64)) : (i64,i64) =
    loop (i,j) while j!=0 do
        (j,i%j)

-- Make PCM with euclid GCD (https://en.wikipedia.org/wiki/Greatest_common_divisor#Least_common_multiple)
def euclid_ppcm((i:i64,j:i64)) : i64 =
    let (x,_) = euclid_pgcd(i,j)
    in (i64.abs(i*j)/x)


-- Example : 
    -- let (i,_) = euclid_pgcd(n[0],n[1])
    -- in i

    -- euclid_ppcm(n[0],n[1])
def main (n:[]i64): i64 =
    