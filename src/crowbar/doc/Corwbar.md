# Crowbar Language

---

## 

demo 1

```
for (i = 1; i < 100; i = i + 1) {
    if (i % 15 == 0) {
        print("FizzBuzz\n");
    } elsif (i % 3 == 0) {
        print("Fizz\n");
    } elsif (i % 5 == 0) {
        print("Buzz\n");
    } else {
        print("" + i + "\n");
    }
}
```

## 程序结构

demo 2

```
print("hello, world\n");
```

demo 3

```
#显示将a与b相加的值，并且作为返回值返回的函数
function hoge(a, b) {
    c = a + b;
    print("a+b.." + c + "\n");

    return c;
}
```

## 数据类型

布尔、整数、实数、字符串、**原生指针**

## 变量

静态无类型、赋初始值即为声明、

```
if(a == 10) {
    b = 10;
}
print("b.." + b);
# 若 a != 10 出错
```

顶层结构的赋值成为全局变量，需显式声明对全局变量的引用

```
global a, b, c;
```


```
a = 10;     ←定义全局变量a的声明

function func() {
    global a;
    a = 20;    ←这里的a是全局变量
}

function func2() {
    a = 30;　 ←这里的a是局部变量
    print("a.." + a + "\n");
}

func();
func2();
print("a.." + a + "\n");
```

## 语句和结构控制

```
# if语句的例子
if(a == 10) {
    # a == 10 时执行
} elsif (a == 11) {
    # a == 11 时执行
} else {
    # a 不为10也不为11时执行
}


# while语句的例子
while (i < 10) {
    # i比10小时，此处循环执行
}

# for语句的例子
for(i = 0; i < 10; i = i + 1) {
    # 这里循环10次
}

# break
# continue
# return
```

## 语句和运算符

| 符号 | 运算 |
| - | - |
| -（单目取负） | 符号的反转 |
| * / % | 乘法、除法、求余 |
| + - | 加法、减法 |
| > >= < <= | 大小比较 |
| == != | 同值比较 |
| && | 逻辑与 |
| \|\| | 逻辑或 |
| = | 赋值 |

## 内置函数

| 函数名 | 功能 |
| - | - |
|print(arg) | 显示arg。arg的类型可以是整数、实数、字符串。 |
| fopen(filename, mode) | 打开一个文件，返回文件指针。mode的可选参数与C语言的fopen()一样（其实就是原封不动的传给了C语言）。 |
| fclose(fp) | 传入fp即关闭文件。 |
| fgets(fp) | 从fp中读出一行字符串并返回。 |
| fputs(str, fp) | 向fp输出字符串，输出时不会自动添加换行。 |

## 支持C调用和调用C

## 整理

token类型
关键字、保留字、常量、符号等

```

```