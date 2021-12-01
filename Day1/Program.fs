module DayOne

let exampleInput =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]


let actualInput =
    [| 104
       105
       109
       120
       124
       113
       120
       121
       122
       123
       134
       133
       134
       150
       160
       165
       179
       178
       180
       178
       196
       197
       198
       204
       197
       213
       206
       207
       209
       210
       211
       223
       212
       224
       228
       238
       237
       238
       242
       245
       249
       250
       253
       248
       261
       265
       274
       276
       274
       275
       278
       275
       276
       267
       266
       272
       275
       279
       280
       286
       282
       292
       308
       323
       324
       319
       310
       304
       309
       310
       320
       326
       322
       340
       341
       340
       339
       344
       346
       342
       353
       352
       353
       349
       346
       330
       342
       343
       344
       329
       348
       346
       367
       373
       379
       384
       388
       398
       387
       392
       391
       385
       409
       411
       416
       419
       420
       423
       422
       420
       421
       422
       421
       422
       441
       438
       428
       431
       443
       427
       433
       434
       432
       421
       429
       432
       434
       439
       437
       445
       456
       462
       464
       472
       474
       460
       433
       439
       433
       444
       456
       457
       452
       454
       455
       465
       474
       475
       486
       489
       490
       491
       472
       477
       475
       493
       497
       495
       496
       488
       500
       501
       504
       505
       524
       525
       522
       530
       548
       545
       562
       570
       582
       583
       584
       586
       585
       604
       606
       610
       617
       619
       621
       623
       620
       622
       624
       625
       633
       627
       623
       638
       639
       644
       643
       644
       648
       663
       695
       681
       682
       686
       692
       708
       712
       720
       736
       737
       738
       739
       738
       741
       742
       741
       746
       748
       751
       750
       782
       788
       793
       799
       790
       834
       852
       860
       848
       856
       857
       859
       860
       863
       849
       844
       854
       860
       862
       865
       864
       866
       887
       878
       899
       893
       895
       896
       897
       876
       877
       886
       887
       898
       905
       906
       907
       875
       877
       875
       894
       893
       871
       878
       879
       872
       868
       873
       884
       900
       899
       886
       896
       903
       909
       928
       932
       935
       928
       913
       917
       919
       920
       912
       917
       923
       926
       927
       932
       936
       944
       942
       961
       960
       961
       956
       951
       971
       980
       990
       999
       1005
       1008
       1010
       1011
       1032
       1034
       1041
       1066
       1067
       1064
       1066
       1070
       1068
       1079
       1100
       1122
       1118
       1111
       1113
       1141
       1145
       1170
       1171
       1170
       1167
       1170
       1171
       1178
       1183
       1186
       1189
       1190
       1198
       1200
       1202
       1204
       1207
       1226
       1243
       1245
       1246
       1247
       1273
       1267
       1274
       1276
       1298
       1293
       1313
       1315
       1298
       1301
       1316
       1322
       1282
       1256
       1257
       1263
       1271
       1294
       1313
       1322
       1323
       1322
       1324
       1309
       1320
       1325
       1321
       1327
       1335
       1334
       1335
       1346
       1382
       1388
       1393
       1395
       1403
       1417
       1424
       1426
       1427
       1428
       1429
       1432
       1431
       1430
       1411
       1439
       1440
       1441
       1458
       1461
       1466
       1471
       1481
       1483
       1486
       1488
       1494
       1490
       1492
       1489
       1533
       1560
       1577
       1582
       1583
       1592
       1594
       1593
       1594
       1599
       1605
       1616
       1619
       1621
       1627
       1651
       1655
       1656
       1655
       1653
       1651
       1652
       1653
       1657
       1658
       1660
       1674
       1680
       1694
       1700
       1698
       1676
       1648
       1644
       1643
       1645
       1652
       1653
       1654
       1660
       1663
       1664
       1668
       1671
       1681
       1687
       1699
       1702
       1705
       1707
       1712
       1739
       1750
       1753
       1755
       1779
       1769
       1792
       1793
       1798
       1828
       1832
       1811
       1844
       1845
       1846
       1848
       1854
       1857
       1859
       1863
       1866
       1862
       1894
       1898
       1875
       1878
       1879
       1884
       1897
       1883
       1894
       1889
       1890
       1889
       1907
       1924
       1926
       1936
       1940
       1949
       1950
       1968
       1973
       1974
       1971
       1982
       1989
       2004
       2005
       2007
       2018
       2032
       2037
       2039
       2040
       2042
       2046
       2056
       2053
       2054
       2057
       2059
       2063
       2067
       2065
       2059
       2049
       2052
       2045
       2053
       2057
       2063
       2066
       2061
       2068
       2063
       2058
       2060
       2061
       2066
       2065
       2093
       2094
       2126
       2135
       2136
       2119
       2121
       2134
       2135
       2137
       2138
       2143
       2144
       2152
       2159
       2154
       2159
       2162
       2164
       2158
       2163
       2162
       2159
       2165
       2166
       2167
       2165
       2169
       2172
       2182
       2188
       2173
       2182
       2175
       2182
       2203
       2205
       2206
       2212
       2213
       2218
       2229
       2226
       2233
       2235
       2233
       2257
       2258
       2284
       2280
       2282
       2285
       2279
       2280
       2291
       2292
       2293
       2294
       2291
       2292
       2293
       2300
       2308
       2303
       2308
       2312
       2311
       2320
       2315
       2326
       2328
       2330
       2352
       2349
       2352
       2357
       2346
       2360
       2375
       2374
       2375
       2377
       2385
       2392
       2383
       2386
       2388
       2403
       2411
       2400
       2401
       2402
       2399
       2400
       2401
       2397
       2417
       2419
       2431
       2430
       2429
       2421
       2429
       2427
       2428
       2429
       2424
       2425
       2426
       2432
       2433
       2435
       2436
       2441
       2454
       2455
       2456
       2464
       2462
       2463
       2465
       2475
       2489
       2495
       2497
       2467
       2484
       2486
       2487
       2502
       2509
       2504
       2503
       2496
       2515
       2516
       2542
       2550
       2552
       2554
       2557
       2559
       2565
       2563
       2552
       2553
       2555
       2552
       2559
       2564
       2565
       2548
       2546
       2536
       2540
       2542
       2543
       2544
       2561
       2550
       2551
       2552
       2556
       2562
       2559
       2596
       2583
       2586
       2584
       2585
       2590
       2592
       2590
       2591
       2599
       2615
       2617
       2595
       2599
       2607
       2605
       2600
       2612
       2611
       2591
       2611
       2590
       2599
       2597
       2604
       2612
       2613
       2620
       2603
       2592
       2604
       2606
       2588
       2591
       2577
       2576
       2606
       2605
       2614
       2618
       2622
       2633
       2636
       2648
       2649
       2638
       2637
       2644
       2651
       2650
       2651
       2658
       2657
       2659
       2661
       2660
       2658
       2669
       2676
       2688
       2691
       2693
       2697
       2698
       2702
       2699
       2706
       2707
       2710
       2698
       2697
       2705
       2717
       2718
       2720
       2721
       2731
       2749
       2757
       2765
       2767
       2759
       2776
       2780
       2784
       2778
       2779
       2783
       2788
       2789
       2825
       2826
       2820
       2825
       2852
       2871
       2870
       2872
       2873
       2878
       2882
       2900
       2906
       2908
       2907
       2917
       2931
       2933
       2931
       2952
       2953
       2954
       2962
       2971
       2973
       2960
       2965
       2967
       2968
       2982
       2983
       2994
       2997
       3014
       3026
       3028
       3040
       3057
       3063
       3064
       3065
       3066
       3073
       3056
       3052
       3067
       3070
       3071
       3072
       3073
       3077
       3078
       3074
       3083
       3082
       3079
       3080
       3081
       3073
       3069
       3072
       3081
       3111
       3109
       3112
       3126
       3127
       3128
       3129
       3135
       3146
       3147
       3149
       3142
       3148
       3152
       3165
       3173
       3201
       3216
       3217
       3218
       3217
       3235
       3236
       3241
       3247
       3248
       3249
       3251
       3252
       3251
       3250
       3252
       3260
       3261
       3259
       3255
       3256
       3257
       3258
       3259
       3264
       3299
       3300
       3304
       3313
       3326
       3324
       3326
       3331
       3340
       3341
       3343
       3348
       3342
       3380
       3381
       3370
       3374
       3390
       3379
       3382
       3383
       3386
       3388
       3382
       3386
       3402
       3400
       3406
       3408
       3407
       3409
       3411
       3412
       3411
       3412
       3422
       3441
       3446
       3451
       3449
       3451
       3480
       3500
       3511
       3520
       3521
       3525
       3548
       3557
       3558
       3560
       3561
       3562
       3570
       3571
       3575
       3571
       3570
       3574
       3576
       3583
       3587
       3594
       3615
       3619
       3620
       3622
       3607
       3599
       3602
       3604
       3607
       3617
       3618
       3623
       3626
       3632
       3637
       3635
       3640
       3641
       3642
       3654
       3668
       3669
       3675
       3670
       3672
       3675
       3684
       3688
       3701
       3699
       3705
       3704
       3718
       3724
       3717
       3734
       3736
       3741
       3729
       3730
       3731
       3722
       3717
       3718
       3711
       3708
       3719
       3737
       3738
       3734
       3740
       3747
       3749
       3740
       3752
       3758
       3784
       3787
       3792
       3800
       3802
       3803
       3805
       3814
       3815
       3813
       3816
       3812
       3815
       3816
       3803
       3799
       3805
       3806
       3809
       3788
       3781
       3783
       3779
       3780
       3786
       3776
       3780
       3790
       3802
       3806
       3789
       3790
       3791
       3804
       3801
       3797
       3823
       3826
       3828
       3831
       3834
       3843
       3867
       3870
       3879
       3883
       3893
       3909
       3934
       3932
       3955
       3961
       3963
       3968
       3994
       4001
       4002
       3999
       4008
       4003
       4006
       3997
       3990
       4020
       4021
       4027
       4031
       4038
       4039
       4043
       4040
       4036
       4049
       4050
       4075
       4077
       4079
       4061
       4060
       4062
       4059
       4050
       4052
       4053
       4094
       4097
       4106
       4107
       4108
       4112
       4118
       4122
       4125
       4124
       4126
       4127
       4129
       4130
       4131
       4139
       4163
       4164
       4169
       4168
       4176
       4177
       4175
       4177
       4183
       4186
       4188
       4194
       4173
       4163
       4168
       4198
       4201
       4202
       4205
       4177
       4178
       4181
       4180
       4211
       4214
       4215
       4218
       4219
       4218
       4215
       4220
       4230
       4231
       4232
       4276
       4281
       4288
       4289
       4291
       4293
       4297
       4304
       4319
       4330
       4337
       4343
       4353
       4360
       4361
       4377
       4402
       4401
       4424
       4429
       4445
       4452
       4456
       4460
       4457
       4434
       4436
       4438
       4441
       4442
       4437
       4439
       4446
       4447
       4457
       4456
       4454
       4455
       4456
       4461
       4473
       4471
       4473
       4481
       4495
       4498
       4504
       4511
       4520
       4541
       4540
       4561
       4572
       4577
       4578
       4577
       4572
       4575
       4582
       4608
       4617
       4623
       4625
       4635
       4640
       4673
       4675
       4676
       4692
       4687
       4688
       4692
       4703
       4685
       4674
       4693
       4708
       4703
       4693
       4681
       4682
       4696
       4722
       4729
       4732
       4739
       4740
       4744
       4750
       4751
       4752
       4751
       4744
       4746
       4749
       4754
       4758
       4748
       4749
       4760
       4759
       4761
       4757
       4767
       4779
       4771
       4772
       4774
       4773
       4791
       4792
       4790
       4797
       4790
       4800
       4810
       4836
       4830
       4826
       4828
       4838
       4836
       4840
       4846
       4848
       4863
       4865
       4867
       4880
       4883
       4892
       4908
       4905
       4909
       4890
       4891
       4882
       4889
       4898
       4900
       4903
       4909
       4913
       4919
       4920
       4922
       4915
       4922
       4923
       4944
       4960
       4978
       4979
       4983
       5006
       5009
       5012
       5011
       5023
       5045
       5010
       5026
       5027
       5030
       5049
       5055
       5053
       5068
       5069
       5077
       5083
       5084
       5087
       5094
       5093
       5101
       5108
       5110
       5111
       5116
       5118
       5121
       5153
       5155
       5156
       5157
       5150
       5171
       5172
       5180
       5191
       5185
       5194
       5218
       5221
       5216
       5224
       5225
       5226
       5225
       5231
       5227
       5228
       5223
       5247
       5245
       5246
       5260
       5287
       5281
       5285
       5287
       5291
       5302
       5303
       5304
       5302
       5310
       5321
       5317
       5326
       5327
       5329
       5307
       5310
       5313
       5336
       5358
       5367
       5369
       5375
       5379
       5368
       5370
       5371
       5372
       5377
       5383
       5384
       5389
       5402
       5404
       5397
       5400
       5402
       5403
       5396
       5400
       5391
       5394
       5400
       5386
       5382
       5389
       5385
       5386
       5388
       5390
       5391
       5397
       5401
       5403
       5405
       5409
       5412
       5431
       5435
       5440
       5457
       5454
       5422
       5423
       5424
       5423
       5426
       5417
       5420
       5424
       5422
       5447
       5446
       5455
       5460
       5472
       5475
       5482
       5483
       5485
       5493
       5494
       5500
       5512
       5519
       5539
       5550
       5554
       5561
       5571
       5597
       5598
       5579
       5584
       5589
       5591
       5592
       5593
       5594
       5616
       5618
       5628
       5630
       5617
       5625
       5626
       5655
       5654
       5663
       5673
       5670
       5669
       5661
       5660
       5665
       5669
       5667
       5675
       5677
       5679
       5680
       5703
       5709
       5710
       5708
       5710
       5711
       5713
       5714
       5715
       5709
       5738
       5714
       5718
       5734
       5735
       5743
       5738
       5739
       5740
       5749
       5752
       5768
       5757
       5753
       5752
       5753
       5754
       5755
       5758
       5770
       5769
       5768
       5781
       5789
       5798
       5799
       5814
       5815
       5824
       5828
       5829
       5830
       5831
       5837
       5835
       5836
       5832
       5820
       5821
       5822
       5814
       5817
       5818
       5821
       5818
       5811
       5814
       5842
       5844
       5863
       5874
       5870
       5883
       5878
       5890
       5891
       5900
       5896
       5895
       5863
       5860
       5861
       5859
       5861
       5865
       5862
       5864
       5870
       5890
       5884
       5889
       5893
       5898
       5902
       5922
       5924
       5926
       5907
       5909
       5915
       5914
       5923
       5925
       5930
       5932
       5934
       5946
       5947
       5950
       5945
       5957
       5965
       5966
       5971
       5967
       5973
       5974
       5977
       5979
       5984
       5991
       5992
       5994
       6002
       6005
       6006
       6011
       6012
       6014
       6000
       6019
       6034
       6053
       6035
       6036
       6042
       6047
       6048
       6050
       6054
       6074
       6075
       6077
       6046
       6055
       6058
       6060
       6061
       6062
       6073
       6081
       6086
       6088
       6096
       6097
       6098
       6099
       6096
       6110
       6112
       6113
       6112
       6111
       6132
       6115
       6118
       6127
       6124
       6128
       6139
       6145
       6156
       6155
       6153
       6152
       6153
       6157
       6164
       6176
       6186
       6187
       6188
       6190
       6192
       6185
       6190
       6191
       6192
       6191
       6198
       6200
       6213
       6214
       6222
       6242
       6236
       6246
       6256
       6257
       6256
       6257
       6258
       6259
       6284
       6285
       6286
       6290
       6291
       6287
       6294
       6298
       6311
       6301
       6309
       6312
       6315
       6318
       6323
       6325
       6334
       6323
       6322
       6323
       6334
       6335
       6332
       6343
       6344
       6348
       6352
       6358
       6361
       6362
       6367
       6381
       6395
       6412
       6411
       6410
       6426
       6444
       6455
       6461
       6470
       6449
       6456
       6455
       6464
       6463
       6468
       6474
       6478
       6477
       6474
       6473
       6468
       6474
       6476
       6501
       6513
       6516
       6517
       6518
       6513
       6515
       6519
       6515
       6531
       6537
       6538
       6531
       6545
       6553
       6554
       6560
       6559
       6562
       6551
       6563
       6559
       6557
       6562
       6564
       6562
       6564
       6563
       6568
       6572
       6573
       6572
       6583
       6591
       6595
       6587
       6589
       6590
       6581
       6583
       6593
       6592
       6616
       6627
       6654
       6657
       6667
       6668
       6680
       6695
       6682
       6685
       6691
       6708
       6720
       6722
       6723
       6721
       6750
       6759
       6760
       6759
       6760
       6769
       6772
       6780
       6774
       6782
       6774
       6777
       6778
       6777
       6786
       6787
       6792
       6796
       6802
       6814
       6816
       6813
       6814
       6823
       6807
       6805
       6804
       6805
       6806
       6807
       6801
       6803
       6806
       6807
       6799
       6812
       6832
       6858
       6846
       6856
       6857
       6865
       6867
       6876
       6877
       6887
       6889
       6895
       6896
       6900
       6901
       6881
       6874
       6873
       6876
       6883
       6888
       6898
       6899
       6910
       6913
       6934
       6932
       6939
       6940
       6943
       6968
       6974
       6978
       6975
       6956
       6971
       6953
       6976
       6983
       6980
       6986
       6987
       6988
       6989
       6992
       7003
       7004
       7005
       7004
       7011
       7017
       7019
       7030
       7033
       7037
       7043
       7053
       7058
       7062
       7076
       7078
       7080
       7093
       7094
       7095
       7107
       7106
       7109
       7110
       7111
       7112
       7113
       7116
       7117
       7119
       7118
       7119
       7120
       7122
       7134
       7145
       7142
       7174
       7175
       7185
       7189
       7190
       7191
       7214
       7228
       7236
       7252
       7267
       7296
       7302
       7303
       7322
       7321
       7323
       7327
       7331
       7333
       7337
       7334
       7343
       7344
       7352
       7361
       7369
       7381
       7384
       7385
       7384
       7389
       7393
       7394
       7395
       7403
       7425
       7430
       7431
       7433
       7441
       7440
       7424
       7427
       7419
       7430
       7432
       7436
       7437
       7450
       7451
       7457
       7477
       7485
       7490
       7496
       7503
       7504
       7508
       7509
       7485
       7490
       7501
       7502
       7506
       7512
       7513
       7514
       7515
       7513
       7528
       7545
       7554
       7563
       7569
       7570
       7573
       7574
       7586
       7589
       7605
       7618
       7620
       7621
       7624
       7645
       7658
       7669
       7672
       7670
       7671
       7678
       7680
       7692
       7712
       7724
       7730
       7736
       7737
       7746
       7759 |]


let countIncreases (input: int[]) =
    let mutable count: int = 0

    for i in 1 .. (input.Length - 1) do
        if input.[i] > input.[i - 1] then
            do
                printfn $"ADDING"
                count <- count + 1

    count

// countIncreases actualInput
