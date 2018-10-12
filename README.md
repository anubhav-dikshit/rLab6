
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.com/anubhav-dikshit/rLab6.svg?branch=master)](https://travis-ci.com/anubhav-dikshit/rLab6)

[![Coverage status](https://codecov.io/gh/anubhav-dikshit/rLab6/branch/master/graph/badge.svg)](https://codecov.io/github/anubhav-dikshit/rLab6?branch=master)

This packages is a fast knapsack solver. For a detailed background of the knapsack problem see [here](https://en.wikipedia.org/wiki/Knapsack%20problem).

Within this vignette, the example data.frame `knapsack_objects` is used to show how to work with the package.

It is a manually created data.frame with 2000 rows:

``` r
head(knapsack_objects)
```

    ##      w        v
    ## 1 3660 4615.520
    ## 2 3749 4137.988
    ## 3 1145 8544.690
    ## 4 3322 1587.847
    ## 5 2567 5226.892
    ## 6 2077 4039.024

Usage of the package
--------------------

The package includes in total three functions which all return the result for a specified knapsack problem.

-   `brute_force_knapsack(x, W)`
-   `knapsack_dynamic(x, W)`
-   `greedy_knapsack(x, W)`

For each function, inputs `x` and `W` have to be specified.

`x` has to be a data.frame with two columns called `v` (for value) and `w` (for weight). `W` is the specified maximum weight of the knapsack.

All algorithms try to find - in a different way - the optimal filling of the knapsack to put in the maximum value considering the specified maximum weight `W`.

### `brute_force_knapsack(x, W)`

``` r
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
```

    ## $value
    ## [1] 12634
    ## 
    ## $elements
    ## [1] 3 8

### `knapsack_dynamic(x, W)`

``` r
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
```

    ## $value
    ## [1] 12634
    ## 
    ## $elements
    ## [1] 3 8

### `greedy_knapsack(x, W)`

``` r
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
```

    ## $value
    ## [1] 12634
    ## 
    ## $elements
    ## [1] 8 3

Speed of algorithms
-------------------

The speed of the algorithms is tested. By doing so, the questions in the lab6 are answered.

### `brute_force_knapsack(x, W)`

Question: How much time does it takes to run the algorithm for n = 16 objects?

``` r
start_time <- Sys.time()
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
```

    ## $value
    ## [1] 18092
    ## 
    ## $elements
    ## [1] 3  8  14

``` r
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 0.07081103 secs

### `knapsack_dynamic(x, W)`

Question: How much time does it takes to run the algorithm for n = 500 objects?

``` r
start_time <- Sys.time()
knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
```

    ## $value
    ## [1] 185809
    ## 
    ## $elements
    ##  [1] 434 462 195 450 214 196 282 229 71  346 345 55  500 290 401 255 288
    ## [18] 234 436 77  37  35  332 110 80  472 117 92

``` r
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 0.04689598 secs

### `greedy_knapsack(x, W)`

Question: How much time does it takes to run the algorithm for n = 1000000 objects?

``` r
start_time <- Sys.time()
greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)
```

    ## $value
    ## [1] 6990826
    ## 
    ## $elements
    ##    [1] 386314 640068 731378 143187 77957  971550 683460 495595 496734
    ##   [10] 88176  567932 602507 300226 485974 218781 529765 893708 412611
    ##   [19] 156272 394157 548722 584004 304118 386920 922389 297653 346531
    ##   [28] 112570 47819  524270 105825 705755 247353 415515 880827 725636
    ##   [37] 646458 629907 234405 728149 336951 696014 891017 793145 478452
    ##   [46] 244050 370544 151872 683326 879948 109338 47148  629033 92    
    ##   [55] 287671 622175 726866 233064 421066 706389 709642 404077 913910
    ##   [64] 322074 730572 580417 791054 453473 807139 152830 351617 308028
    ##   [73] 966336 732857 353944 883431 810704 40622  824796 906340 11846 
    ##   [82] 55489  310816 434874 572045 305414 349680 818388 770926 689043
    ##   [91] 157922 403408 296316 423987 791906 508985 529571 160089 637530
    ##  [100] 785390 930034 278376 379537 20762  614435 173648 772263 623272
    ##  [109] 194514 604457 722205 356185 813501 904604 755053 905225 425019
    ##  [118] 886096 361179 895040 707779 520011 212866 623584 174080 291327
    ##  [127] 585450 82234  436829 584020 116502 690666 728190 66195  927579
    ##  [136] 986844 381640 522470 126058 123683 310408 110543 993855 719613
    ##  [145] 999970 988323 261903 508451 607175 40233  106167 69820  4064  
    ##  [154] 649780 9820   99029  198927 129810 171792 532125 833221 764619
    ##  [163] 813887 875686 937153 234591 878443 747608 460808 370314 952837
    ##  [172] 146461 472014 798100 559388 111445 143732 528564 116630 213962
    ##  [181] 80696  531255 407458 200683 577824 511315 69610  712714 346291
    ##  [190] 750560 320899 331515 856810 503927 393509 612842 974281 363667
    ##  [199] 764075 746086 60389  357306 412105 59997  119546 581154 169702
    ##  [208] 638102 68272  523424 608553 988530 130158 430454 548905 81756 
    ##  [217] 129597 566645 166408 448773 709017 385803 442566 949215 804049
    ##  [226] 1959   65289  704563 905056 601386 772615 910991 805644 293190
    ##  [235] 826215 338203 96380  417413 799296 689001 43221  403832 639924
    ##  [244] 910755 392239 860370 697172 67123  79992  112585 389186 637546
    ##  [253] 312314 523511 214436 273611 743263 698967 473833 154531 821788
    ##  [262] 100592 603005 663475 942550 349961 509378 672881 337886 187090
    ##  [271] 111440 24767  341314 409558 964598 131919 273353 365661 900184
    ##  [280] 316988 538877 319422 266467 353861 539901 813720 499313 365433
    ##  [289] 518157 836001 628349 512351 478003 944780 244642 682067 619786
    ##  [298] 323864 911781 320009 811931 941304 817138 825286 693551 233037
    ##  [307] 337827 593475 31259  990046 334647 390698 653221 829261 690000
    ##  [316] 368567 71232  767376 289066 772061 359927 704206 45626  612004
    ##  [325] 498161 794958 929983 49668  555985 480520 761853 646012 270313
    ##  [334] 49751  104005 29632  108621 253959 338273 786590 63363  381724
    ##  [343] 320860 605077 229393 709802 517851 817531 546815 33447  203662
    ##  [352] 401404 498931 5847   616374 200960 567177 7641   772937 367080
    ##  [361] 771735 178397 84914  474437 141740 45400  918933 574950 55452 
    ##  [370] 671965 844176 767073 287553 245973 192366 662701 35395  272464
    ##  [379] 130634 172311 933599 523814 65183  936938 956477 789474 73448 
    ##  [388] 172410 711470 356314 663118 28419  639841 663546 944129 38586 
    ##  [397] 773546 359815 743647 980799 28368  795854 90385  462358 579515
    ##  [406] 431436 886211 713930 216084 574    261136 924509 658423 269544
    ##  [415] 310553 382239 212406 364491 633729 193920 802462 983753 610015
    ##  [424] 664515 792838 887690 214203 44739  764054 343383 441692 754792
    ##  [433] 576400 296445 33111  20112  542328 957924 683505 802917 730561
    ##  [442] 940849 337584 778285 866708 256710 372555 717622 225028 743652
    ##  [451] 550668 850334 15234  112870 197546 788162 43697  218221 239339
    ##  [460] 999323 840302 776431 605633 656910 855078 199914 149665 47529 
    ##  [469] 327639 654843 70079  952695 842143 353823 687622 599838 562045
    ##  [478] 725261 141453 522850 97466  165128 250593 119928 994386 6636  
    ##  [487] 493524 537954 70143  413642 520630 877058 438692 655881 13895 
    ##  [496] 649897 590406 55803  283416 669105 60759  330846 302657 872837
    ##  [505] 102060 493220 798360 20449  383227 335246 952159 303605 930746
    ##  [514] 410237 828438 209734 667517 668696 637446 863929 200179 993562
    ##  [523] 14423  133262 337509 633363 204585 76329  292474 381928 970124
    ##  [532] 181087 881297 30648  575410 147290 811353 304885 920298 189692
    ##  [541] 362673 476202 68143  12653  427764 196999 575731 201411 211961
    ##  [550] 948622 68543  476933 839010 86032  961590 245397 152894 981239
    ##  [559] 911832 351030 374763 865579 434201 193182 854839 284016 446772
    ##  [568] 971244 886550 976900 589751 861381 439998 255689 447929 15761 
    ##  [577] 41808  818531 60275  958989 813332 328644 546807 317382 150049
    ##  [586] 359714 996055 177437 112579 609118 69113  944521 535648 711941
    ##  [595] 823339 878106 820950 588326 73872  638385 162323 761935 725884
    ##  [604] 327413 281747 352048 412848 754984 467352 516065 320199 886009
    ##  [613] 182532 697394 70740  278088 864222 41579  875569 454032 255787
    ##  [622] 290861 485663 373841 792726 35505  342821 471086 929952 467854
    ##  [631] 510057 618914 917726 328221 354549 203454 338663 443785 617697
    ##  [640] 221438 494724 127526 285759 724842 717812 735540 31525  53599 
    ##  [649] 538116 77301  402047 986911 748356 235948 131638 412439 728388
    ##  [658] 924857 727983 639529 320132 944335 828324 803884 831266 453316
    ##  [667] 377259 140032 401297 301634 175464 756193 802701 541959 650723
    ##  [676] 238119 167749 836373 955939 183335 858459 249622 338799 305381
    ##  [685] 419474 394628 309118 67538  962775 869430 238310 116997 65327 
    ##  [694] 940850 6896   436834 625563 897071 745551 221857 987053 189534
    ##  [703] 195748 583417 27606  284718 693784 284422 710856 613829 682243
    ##  [712] 652834 549197 488607 986829 496095 124774 371620 501714 784212
    ##  [721] 516126 620252 322717 277464 601640 840738 44796  212221 81423 
    ##  [730] 445861 506642 171373 239643 345444 698084 624614 392138 387813
    ##  [739] 306663 47034  676960 638986 453333 273319 379652 911942 354644
    ##  [748] 286354 384953 119678 247390 948018 79028  265087 472619 593841
    ##  [757] 655067 485440 211202 402816 982363 511361 707725 42262  34991 
    ##  [766] 297896 262818 635538 957829 553375 820468 736647 750492 519779
    ##  [775] 697460 411612 37583  144339 379486 752909 627122 576576 615373
    ##  [784] 488728 523324 865141 484811 103955 586680 828069 470153 131024
    ##  [793] 884485 581749 961114 551444 483059 530258 787890 176596 477058
    ##  [802] 101948 40219  789330 222299 245588 107630 426225 46942  849751
    ##  [811] 276267 323270 532854 846809 879822 340711 87881  49616  708991
    ##  [820] 966034 989545 366993 127438 257092 247202 164829 310727 799359
    ##  [829] 42587  166098 458355 69750  168357 453556 158300 768093 539481
    ##  [838] 671745 779525 836062 673469 702736 731264 242762 981293 753417
    ##  [847] 672892 266193 342757 1398   169084 35941  491871 46657  681132
    ##  [856] 198094 231700 814736 364398 539768 641157 371157 545417 672837
    ##  [865] 970758 66828  227726 242609 974881 899510 870540 490741 893478
    ##  [874] 594069 62754  620434 768729 380781 332241 770465 981807 717169
    ##  [883] 877617 55708  719430 871300 508045 608482 827472 723989 965775
    ##  [892] 670247 20981  306868 788426 409005 698667 840704 735774 57974 
    ##  [901] 452387 115466 303046 969150 234323 412654 966856 315752 292943
    ##  [910] 213781 473114 872857 616230 93333  408923 918376 844095 400063
    ##  [919] 570387 790637 659233 665963 210076 875441 924670 759266 37210 
    ##  [928] 967082 984274 777801 334512 10628  290965 481931 208464 56549 
    ##  [937] 433721 834334 78984  855787 896612 935077 630199 880665 585021
    ##  [946] 752511 307857 390988 664667 401219 501835 44133  835697 596766
    ##  [955] 557336 737749 317827 371778 138921 465567 194476 437206 699276
    ##  [964] 350037 894625 235464 457901 200674 174304 905557 242863 600927
    ##  [973] 222559 474358 878125 304576 21816  177711 180958 251201 674251
    ##  [982] 153640 420423 656097 15860  752838 715388 389235 462395 208954
    ##  [991] 204511 626844 394060 307112 182080 468617 67660  456073 822859
    ## [1000] 68836  99820  95438  373545 514132 450677 194347 585050 865133
    ## [1009] 631883 468912 742426 382495 932937 522506 405865 247215 128088
    ## [1018] 900915 87366  18132

``` r
end_time <- Sys.time()
end_time - start_time
```

    ## Time difference of 0.5603802 secs