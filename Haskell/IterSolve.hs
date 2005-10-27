-- IterSolve.hs
-- Evaluate the root of an equation using
--   Simple Iteration Method
-- Copyright (c) 2005 Agent Zhang
-- 2005-09-18 2005-09-19

module IterSolve where

repeat' x next = x : repeat' (next x) next

isolve x0 next cond = cond $ repeat' x0 next

within eps (a:a':as)
    | abs(a-a') <= eps = a'
    | otherwise        = within eps (a':as)

within' eps (a:a':as)
    | abs(a-a') <= eps = 1
    | otherwise        = 1 + within' eps (a':as)

relative eps (a:a':as)
    | abs(a-a') <= eps * abs a' = a'
    | otherwise                 = relative eps (a':as)

relative' eps (a:a':as)
    | abs(a-a') <= eps * abs a' = 1
    | otherwise                 = 1 + relative' eps (a':as)

fi x = sqrt (2*x + 3)

test1 = isolve 4 fi (within 0.01)
test1' = isolve 4 fi (within' 0.01)

fi' x = 1/2 * (x^2 - 3)

test2 = isolve 4 fi' (within 0.01)
test2' = isolve 4 fi' (within' 0.01)

g x = 1 + 1/x^2

test3 = isolve 1.5 g (relative 0.00005)
test3' = isolve 1.5 g (relative' 0.00005)

test4 = isolve 1.5 g (within 0.0005)
test4' = isolve 1.5 g (within' 0.0005)
test4'' = take 13 (repeat' 1.5 g)

g' x = (1 + x^2)**(1/3)

test5 = isolve 1.5 g' (within 0.0005)
test5' = isolve 1.5 g' (within' 0.0005)
test5'' = take 7 (repeat' 1.5 g')

h x = sqrt (x^3-1)

test6 = isolve 1.5 h (within 0.0005)
test6' = isolve 1.5 h (within' 0.0005)

h' x = 1 / sqrt (x-1)

test7 = isolve 1.5 h' (within 0.0005)
test7' = isolve 1.5 h' (within' 0.0005)

{-

求方程 x^3 - x^2 - 1 = 0 在 x0 = 1.5 附近的根，将其改写为如下 4 种不同的等价形式，构造相应的迭
代格式，试分析它们的收敛性。选一种收敛速度最快的迭代格式求方程的根，精确至 4 位有效数字。

我们在这里再一次给出 Haskell 实现。事实上，迭代法求根比二分法更为简单。当然，用于产生 x 近似根序列
的递推公式（即迭代格式）是由用户通过函数参数提供的。由于这儿只是简单地由前一个 x 生成 后一个 x，
repeat' 的实现便只有一行：

    repeat' x0 next = x0 : repeat' (next x0) next

这样，迭代法求根的用户接口 isolve 函数的定义就自然而然地出来了：

    isolve x0 next cond = cond $ repeat' x0 next

这里，参数 x0 是 x 的初始值，next 是用户提供的具体的迭代格式，cond 是用户自定义的“精度要求”（即
通常意义上的迭代终止条件），事实上，我们完全可以将二分法求根 BinSolve.hs 中的 cond 的典型实现
within 和 relative 的实现直接照搬过来：

    within eps (a:a':as)
        | abs(a-a') <= eps = a'
        | otherwise        = within eps (a':as)

    relative eps (a:a':as)
        | abs(a-a') <= eps * abs a' = a'
        | otherwise                 = relative eps (a':as)

现在我们就可以利用这些 Haskell 代码，通过本题中的迭代式（1）来计算方程的根了：

    g x = 1 + 1/x^2
    test3 = isolve 1.5 g (relative 0.00005)

在 Hugs 中调用 test3 的情形如下：

    IterSolve> test3
    1.46559498495446

舍入后我们得到方程的根为 1.466

我们在 Maple 9 中对此结果进行验证：

    > evalf(solve(x^3-x^2-1=0,x));
      1.465571232, -0.2327856159 + 0.7925519930 I, -0.2327856159 - 0.7925519930 I

1.466 显然在解集中。

另一种验根的更直接的方法是把求得的近似根代入到迭代式中：

    IterSolve> g 1.466 - 1.466
    -0.000701064045606659

我们看到代入后的结果已经相当接近 0 了。

如果我们现在想得到刚才进行的迭代的次数，我们需要给出 relative 函数的另一种实现（由于在
Functional Programming 中不能使用“副作用”来做一些额外的事情）：

    relative' eps (a:a':as)
        | abs(a-a') <= eps * abs a' = 1
        | otherwise                 = 1 + relative' eps (a':as)

此时我们可以用这个函数来作为 isolve 的 cond 参数进行调用：

    test3' = isolve 1.5 g (relative' 0.00005)

得到的结果为

    IterSolve> test3'
    16

即需要迭代 16 次才能得到前面的计算结果 1.46559498495446. 这里有一个有趣的地方是，我们
通过改变传入 isolve 函数的 cond 参数来改变 isolve 求根函数的行为。relative' 使用
isolve 不再返回最后求得的近似根，而是返过迭代的次数。我们在此看到了 OO 的多态特性的一些
影子。

前面我们使用 0.00005 （即 0.5/10^4）作为迭代的相对误差限，即迭代终止条件来求根，这样可
以保证结果的 4 位有效数字的精度。但这种误差限设立得有些过于保守了，它几乎总会导致过多的迭
代操作。如果我们知道了根的数量级大小，我们就可以给出比较准确的绝对误差限来避免冗余运算。

由题意知，我们要求的根就在 1.5 附近，故根的数量级为 1，即 10^0，于是我们可以将绝对误差限
 eps 设定为 0.5 * 10^-3，即 0.0005：

    test4 = isolve 1.5 g (within 0.0005)

Hugs 的计算结果为：

    IterSolve> test4
    1.46571701802245

舍入后的值仍为 1.466. 与前面得到的结果完全一致。

仿照前面定义 relative' 的方法定义 within'，以便得到实际迭代的次数：

    within' eps (a:a':as)
        | abs(a-a') <= eps = 1
        | otherwise        = 1 + within' eps (a':as)

此时调用

    test4' = isolve 1.5 g (within' 0.0005)

的结果为

    IterSolve> test4'
    12

我们看到，迭代次数比使用 0.00005 作为相对误差限来求解减少了 4 次。本次求根过程得到的
x 近似值的序列可以用下面的命令得到：

    test4'' = take 13 (repeat' 1.5 g)

Hugs 的输出为（经过重新排版）：

    [1.5,1.44444444444444,1.4792899408284,1.456976,1.47108058332003,1.46209053547124,
    1.46779057601959,1.46416438046218,1.46646635571707,1.46500304056686,1.46593243908183,
    1.46534182571779,1.46571701802245]

接下来我们利用 isolve 函数来尝试本题的迭代式（2）：

    g' x = (1 + x^2)**(1/3)
    test5 = isolve 1.5 g' (within 0.0005)

得到的结果为

    IterSolve> test5
    1.46587682016881

我们再来看看这一次求根的迭代次数：

    test5' = isolve 1.5 g' (within' 0.0005)

得到

    IterSolve> test5'
    6

哇，只迭代了 6 次耶！比使用迭代式（1）整整减少了一半的迭代量！

下面我们来获取 x 近似值的序列：

    test5'' = take 7 (repeat' 1.5 g')

其输出如下：

    IterSolve> test5''
    [1.5,1.48124803420369,1.47270572963939,1.4688173136645,1.4670479732006,
    1.46624301011475,1.46587682016881]

通过理论计算我们知道，迭代式（3）与（4）是发散的；我们不妨看看用 isolve “强行”求解会
出现什么情况（我已做好按下 Ctrl-C 的准备）：

    h x = sqrt (x^3-1)
    test6 = isolve 1.5 h (within 0.0005)

很令人吃惊，下面的调用并没有导致预想中的“无限循环”！

    IterSolve> test6
    1.#INF

我在按下回车键后就得到了结果，但这个结果看起来确实很奇怪。事实上，它表示浮点数发生了溢出。
IEEE 能表示的数的范围究竟是有限度的，而迭代很快就用完了所有的“资源”。

我们最后再试试迭代式（4）：

    h' x = 1 / sqrt (x-1)
    test7 = isolve 1.5 h' (within 0.0005)

这一回我们什么结果都没有得到，Hugs 生成了如下所示的出错消息：

    IterSolve> test7

    Program error: argument out of range

通过检查 x 序列，我注意到在迭代了若干次之后，x 的值就超出了迭代格式（4）的定义域，即被
开方数 x-1 变成负数了，于是便导致了程序错误：

    IterSolve> take 10 (repeat' 1.5 h')
    [1.5,1.41421356237309,1.55377397403004,1.34379719253106,1.70548866384199,
    1.19057012381854,2.29072308199971,0.880204251020792,
    Program error: argument out of range

使用 Haskell 语言进行数值方法课程的算法实现与研究真的十分惬意。试想如果用 C/C++ 来做这
些活儿，生活会变成什么样子！我从前从未想到数值方法的学习与编程实践竟然可以如此有趣！

-}