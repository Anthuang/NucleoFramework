# Nucleo Framework
Framework in Mathematica for communication with other platforms for robotic applications.

The goal is to provide a platform where the user can perform both control and data analysis together.

Please read the [documentation file](https://github.com/Anthuang/NucleoFramework/blob/master/SerialDocumentation.pdf) for more information.

## Purpose
We aim to provide a platform where the user can perform tasks, control, and data analysis together. This platform will be a better environment for researchers and programmers alike to interact with robots.
![purpose](https://github.com/Anthuang/NucleoFramework/blob/master/purpose.png)

## Current Functionality
- [x] Serial communication
- [x] Simple GUI and graphing
- [ ] Improve speed of communication
- [ ] Communication between Mathematica and multiple boards
- [ ] Communication between Mathematica and simulation applications

## Serial Protocol
All messages must follow the following format:

**[Start][ID][Length][Instruction][Parameter 1]...[Parameter N][Checksum]**
![flowchart](https://github.com/Anthuang/NucleoFramework/blob/master/serial_flowchart.png)

## IMU Graphing Demo
![demo](https://github.com/Anthuang/NucleoFramework/blob/master/imudemo.gif)
