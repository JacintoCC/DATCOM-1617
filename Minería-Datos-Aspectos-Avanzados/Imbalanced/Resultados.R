metodos <- paste("Modelo", 2:10, ".R", sep = "")
sapply(metodos, source)

# > print("SMOTE")
# [1] "SMOTE"
# > print(auc.measures)
# $SVM
# kernel   results
# 1     linear 0.8238314
# 2 polynomial 0.7831785
# 3    sigmoid 0.7248683
# 4     radial 0.8309261
# 
# $RF
# ntree   results
# 1   400 0.8328468
# 2   500 0.8345428
# 3   600 0.8351328
# 
# $KNN
# k   results
# 1 10 0.6698427
# 2 50 0.6624054
# 
# $XGB
# nrounds   results
# 1      40 0.8222032
# 2      60 0.8289325
# 3      80 0.8233495
# 4     100 0.8290153
# 
# $BAG
# mfinal   results
# 1     75 0.7945128
# 2    100 0.7857267
# 
# [1] "ENN"
# $SVM
# kernel   results
# 1     linear 0.8253396
# 2 polynomial 0.7993823
# 3    sigmoid 0.7424591
# 4     radial 0.8389659
# 
# $RF
# ntree   results
# 1   400 0.8361757
# 2   500 0.8369618
# 3   600 0.8361767
# 
# $KNN
# k   results
# 1 10 0.7758336
# 2 50 0.7758336
# 
# $XGB
# nrounds   results
# 1      40 0.8321122
# 2      60 0.8303215
# 3      80 0.8299503
# 4     100 0.8288287
# 
# $BAG
# mfinal   results
# 1     75 0.8057975
# 2    100 0.8100951
# [1] "NCL"
# $SVM
# kernel   results
# 1     linear 0.8253546
# 2 polynomial 0.8009166
# 3    sigmoid 0.7494102
# 4     radial 0.8422837
# 
# $RF
# ntree   results
# 1   400 0.8404914
# 2   500 0.8411968
# 3   600 0.8400157
# 
# $KNN
# k   results
# 1 10 0.7805774
# 2 50 0.7805774
# 
# $XGB
# nrounds   results
# 1      40 0.8357246
# 2      60 0.8338839
# 3      80 0.8311527
# 4     100 0.8325065
# 
# $BAG
# mfinal   results
# 1     75 0.8159403
# 2    100 0.8150987

# [1] "CNN"
# $SVM
# kernel   results
# 1     linear 0.8258988
# 2 polynomial 0.7978705
# 3    sigmoid 0.7340943
# 4     radial 0.8392200
# 
# $RF
# ntree   results
# 1   400 0.8360509
# 2   500 0.8377549
# 3   600 0.8368976
# 
# $KNN
# k   results
# 1 10 0.7715381
# 2 50 0.7722134
# 
# $XGB
# nrounds   results
# 1      40 0.8291218
# 2      60 0.8302987
# 3      80 0.8251918
# 4     100 0.8271935
# 
# $BAG
# mfinal   results
# 1     75 0.7993171
# 2    100 0.8073439
# [1] "OSS"
# $SVM
# kernel   results
# 1     linear 0.8262638
# 2 polynomial 0.8003975
# 3    sigmoid 0.7375804
# 4     radial 0.8429877
# 
# $RF
# ntree   results
# 1   400 0.8392520
# 2   500 0.8391495
# 3   600 0.8391089
# 
# $KNN
# k   results
# 1 10 0.7843129
# 2 50 0.7843774
# 
# $XGB
# nrounds   results
# 1      40 0.8337612
# 2      60 0.8331080
# 3      80 0.8317788
# 4     100 0.8341171
# 
# $BAG
# mfinal   results
# 1     75 0.8121213
# 2    100 0.8167638
# [1] "Oversampling"
# $SVM
# kernel   results
# 1     linear 0.8261821
# 2 polynomial 0.7952133
# 3    sigmoid 0.7284552
# 4     radial 0.8346080
# 
# $RF
# ntree   results
# 1   400 0.8356587
# 2   500 0.8383011
# 3   600 0.8400599
# 
# $KNN
# k   results
# 1 10 0.6443075
# 2 50 0.6445344
# 
# $XGB
# nrounds   results
# 1      40 0.8289808
# 2      60 0.8268720
# 3      80 0.8257880
# 4     100 0.8281017
# 
# $BAG
# mfinal   results
# 1     75 0.8092402
# 2    100 0.8102495

# [1] "Undersampling"
# $SVM
# kernel   results
# 1     linear 0.8235894
# 2 polynomial 0.8017255
# 3    sigmoid 0.7491885
# 4     radial 0.8371780
# 
# $RF
# ntree   results
# 1   400 0.8373471
# 2   500 0.8379672
# 3   600 0.8382105
# 
# $KNN
# k   results
# 1 10 0.7636711
# 2 50 0.7664944
# 
# $XGB
# nrounds   results
# 1      40 0.8315763
# 2      60 0.8245225
# 3      80 0.8262220
# 4     100 0.8272250
# 
# $BAG
# mfinal   results
# 1     75 0.8162845
# 2    100 0.8173303

