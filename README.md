# LunarEclipse_U1

## 程序说明

本程序在 gfortran 11.2.0下编译通过！

(1) bin

可执行文件目录

(2) input

输入文件目录，内部包含二进制的DE星历文件“JPLEPH421”

(3) lib

静态库目录，包含两个静态库，一个是sofa包的静态库"libsofa_c_20210512.a"和利用testeph中的两个Fortran代码生成的静态库“libtesteph.a"

(4) src

源码文件目录，其中main.c是主函数文件，其余是自定义函数。

(5) 程序运行结果.png

可执行文件运行结果截图。

## 编译方法

打开终端，将路径切换至src下，执行：

```bash
make
```

或

```bash
make -f makefile
```

## 程序运行方法

打开终端，将路径切换至bin下，执行：

```bash
./EclipseFirst
```
