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

(6) makefile

整个程序的`makefile`，执行该`makefile`会首先编译两个需要用到的静态库 `libtesteph.a` `libsofa_20210512.a`，再编译生成主程序`EclipseFirst`。

首次编译时，请先执行该makefile，之后调试代码可只执行`src`文件夹下的`makefile`。

## 编译方法

### 第一次编译

打开终端，将路径切换至当前目录下，执行：

```bash
make
```

或

```bash
make -f makefile
```

### 调试代码

第一次编译完成后，后续如果只对`src`文件夹里的源代码进行修改调试，则只需要执行`src`文件夹下的`makefile`，即可，方法与上文所述类似。

## 程序运行方法

打开终端，将路径切换至bin下，执行：

```bash
./EclipseFirst
```
