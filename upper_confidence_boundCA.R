# Upper Confidence Bound

# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')
#This shows if a customer clicked on an ad (1) or didn't (0)

#We want to know the ad that will be clicked the most
#We place 1 ad each round, for 10,000 rounds
#We won't select them randomly though
#Each selection will depend on the previous results
#This dataset shows what versions the user will click (What God knows)

# Implementing Random Selection
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0
for (n in 1:N) {
  ad = sample(1:10, 1)
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  total_reward = total_reward + reward
}
#This algorithm (pre-made) just selects them randomly
#Total correct selections = total reward = ~1220 for random selection

# Visualising the results of random selection
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')
#This shows each ad was clicked about the same # of times


# Implementing UCB
#No package, we're coding from scratch
N = 10000
d = 10
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
ads_selected = integer(0)
total_reward = 0

#N = 10000 rounds
#d = 10 rounds
#integer(d) makes a vector of 10 zero's, since d = 10
#b/c at the start of round 1 each ad has been selected 0 times
#sum of rewards at round 0 is also a vector of 0's
#ads_selected = integer(0) is an empty vector
##if we used ads_selected = integer(d), the algo would try to 
##select an ad version 0, which does not exist
##I did this by mistake, it does not change the overall outcome

for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      upper_bound = 1e400
    }
      if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}


#This^ will be the strategy for rounds 11-10000
#For the first 10 rounds, we will select each ad in order (1-10)
#Then the code starts at round 11, so it has some data to work from

#New total rewards is ~2183, much more clicks
#Look at ads_selected vector in console to see which was picked for each round
#The last rounds should have many of the same ad, since it's the best
#We see lots of 5's if we use  ads_selected[9900:10000] to see just last rounds
#So version 5 is the best ad

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')

#No doubt, ad 5 is best
