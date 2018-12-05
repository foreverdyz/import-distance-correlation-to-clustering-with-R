# import-distance-correlation-to-clustering-with-R
I want to make a new algorithm based on traditional clustering algorithm with a new statistcs value, named distance correlation./n
The first-version is different from k-means with the distance correlation./n
The second-version is a clustering algorithm which do not need a center in each cluster but try to make every cluster not more indepedent./n
The third-version is a clustering algorithm which combine some knowledge about convex and clustering. We do not need set the superparameter k whcih is the number of clusters need we give before the programm begin. Convexity can guarantee the uniqueness of the result in some way so our new algorithm can decide the number of clusters automatic. Although there are some noise points in the result such as only 2 points in one cluster when there are 150 points in all, we think we can analysis the result and correct it.
