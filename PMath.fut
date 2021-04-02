-- ---
-- title: Minimal Personnal Library for math usage
-- ---

-- Simple entry for library porting
entry my_power (n: f32,m: f32): f32 = n**m

-- Simple entry for library porting
entry my_divisor (n: f32, m: f32): f32 = n/m


-- Iterate test
-- let main(n : u64): (u64,u64) =
--     let tmp_p = 2
--     let tmp_q = 7
--     in iterate_until(\(p,q) -> p == q)(\(p,q) -> (p,q-1))(tmp_p,tmp_q)


-- Test if number is prime
let is_prime(n: i64): bool =
    all(\elem -> if n%elem != 0 || n == elem then true else false  ) (map(+1)( tabulate(n**1/2)(\i -> i+1) ))


-- List all primal number from 2 to n
let lst_al_prime_number(n: i64): []i64 =
let res = map(+1)(tabulate(n-1)(\i -> i+1))

in filter(\temp ->
  all(\elem ->
    if temp%elem != 0 || temp == elem then
      true
    else
      false  )
    (map(+1)(tabulate(n**1/2)(\i -> i+1))))

  (res)

-- Compute PGCD of 2 number and return value
let pgcd(n: i64,m: i64): i64 =
  let (a,res) = iterate_until(\(tmp_n,tmp_p) -> if tmp_n%tmp_p==0 then true else false  )
               (\(tmp_n,tmp_p) -> (tmp_p,tmp_n%tmp_p))
               (n,m)
  in res
