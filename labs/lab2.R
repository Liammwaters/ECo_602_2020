#Q1
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)


vec_2 = vec_1 == 3
head(vec_2)
vec_1[vec_2]

    #Q1: vec_2 = vec_1 == 3
    #Q2: It would be time consuming for one thing, and prone to human error since it is a large vector. Much simpler, quicker, neater, and more accurate to make a function do the work



#Q2

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)

sum(vec_1 == 3)



n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))


#Q1: The logical test is able to quickly and accurately tell how many entries there are with a value of 3 each time you run the vector. It is fairly fail safe in terms of there not being much room for error since it is a simple code.
#Q2: Manually choosing by visual inspection just seems like a nightmare. When viewing large data sets, it is difficult (or impossible) to view all of it, and extremely difficult to navigate. Then there is a lot of room for human error and the unreliability of human detection. It would take forever to count all the entries manually, and so it would be cumbersome to rerun the vector more than once. 



#Q3


for (i in 1:10)
{
  print( 
  paste0(
    "This is loop iteration: ", i))
}






#Q4
n = 14

for (i in 1:n)
{
  print(i)
}




#Q5

n = 17
vec_1 = 1:n
vec_1
#i think I can delete everything above this

{
n = 17
vec_1 = sample(10, n, replace = TRUE)
print( paste0("The element of vec_1 at index ", 1:n, " is: ", vec_1))
vec_1
}



#Q6

create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_1 = sample(min:max, n, replace = TRUE)
  for (i in 1:n) 
    {
    print( 
      paste0(
        "The element of vec_1 at index ", i, " is: ", vec_1[i]))
    }
}

create_and_print_vec(10)

create_and_print_vec(10, min = 100, max = 2000)
