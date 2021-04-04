-- ---
-- title: Minimal Personnal Library for math usage
-- ---

-- Simple entry for library porting
let my_power (n: f32,m: f32): f32 = n**m

-- Simple entry for library porting
let my_divisor (n: f32, m: f32): f32 = n/m


-- Iterate test
-- let main(n : u64): (u64,u64) =
--     let tmp_p = 2
--     let tmp_q = 7
--     in iterate_until(\(p,q) -> p == q)(\(p,q) -> (p,q-1))(tmp_p,tmp_q)


-- Test if number is prime
let is_prime(n: i64): bool =
    any(\elem -> if n%elem != 0 || n == elem then true else false  ) (map(+1)( tabulate(n**1/2)(\i -> i+1) ))


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


let little_fermat(n: i64,m: i64): i64 = 
    ((n-1)*(m-1))


-- Compute PGCD of 2 number and return value
let pgcd(n: i64,m: i64): i64 =
  let (_,res) = iterate_until(\(tmp_n,tmp_p) -> if tmp_n%tmp_p==0 then true else false  )
               (\(tmp_n,tmp_p) -> (tmp_p,tmp_n%tmp_p))
               (n,m)
  in res

-- Test for value in finding_p_q
let find_p_q_check_value(n: i64, e: i64, tmp_p: i64, tmp_q: i64): bool = 
    if  (pgcd(tmp_q,tmp_p) == 1 && (tmp_p*tmp_q)==n) then
      if (is_prime(tmp_q) || is_prime(tmp_p)) then
        if pgcd(e,little_fermat(tmp_p,tmp_q))==1 then
            true
        else
            false
      else 
        false
    else
        false

-- Find p q from (n,e) of RSA message
let find_p_q(n: i64, e:i64): (i64,i64) = 
    let (p,q) = iterate_until(\(tmp_p,tmp_q) -> find_p_q_check_value(n,e,tmp_p,tmp_q) )
                              (\(tmp_p,_) -> (tmp_p+1, n / tmp_p) )(2,n/2)
    in (p,q)

let main(_: i64): bool = --(i64,i64) = 
    --find_p_q(7266118032323,827)
    is_prime(7266118032323)