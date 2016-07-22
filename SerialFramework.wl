(* ::Package:: *)

(* ::Title:: *)
(*Serial Framework*)


BeginPackage["SerialFramework`"];


ConnectDevice::usage = "Connects through serial to DeviceAddress. Saves the connection to a global variable. Sets the baud rate to BaudRate.

DeviceAddress depends on the operating system of the computer the device is connected to:
Windows: \[OpenCurlyDoubleQuote]COM#\[CloseCurlyDoubleQuote]
MacOS or Linux: \[OpenCurlyDoubleQuote]/dev/ttyXX\[CloseCurlyDoubleQuote] or \[OpenCurlyDoubleQuote]/dev/tty.usbserialXX\[CloseCurlyDoubleQuote] or something similar

Default value of WriteMessageArgs: \[OpenCurlyDoubleQuote]\[CloseCurlyDoubleQuote] - does nothing.

Default value of BaudRate: 9600 - default baud rate supported by most boards.
";
WriteMessage::usage = "WriteMessage[ WriteMessageArgs_: \[OpenCurlyDoubleQuote]\[CloseCurlyDoubleQuote] ]
Send a write request to the connection global variable with the arguments as WriteMessageArgs. WriteMessage writes an instruction packet with the form [Start][ID][Length][Instruction][Parameter 1]...[Parameter N][Checksum] through serial. WriteMessageArgs are its parameters. The arguments are sent one after the other.

WriteMessageArgs is an array of integers or floats.

Default value of WriteMessageArgs: \[OpenCurlyDoubleQuote]\[CloseCurlyDoubleQuote] - nothing is written.
";
ReadMessage::usage = "Send a read request from the connection global variable with the argument as ReadMessageArg. ReadMessage writes an instruction packet with the form [Start][ID][Length][Instruction][Parameter][Checksum] through serial. ReadMessageArg is its only parameter. ReadMessageArg is an integer which is parsed in mbed. Depending on the integer, mbed can return different information through serial. ReadMessage returns values received from serial after the read request is sent.

Default value of ReadMessageArg: 0 - reads all information from the board.

List of available arguments:
0 - reads all information from the board.
2 - gyroscope information
3 - accelerometer information
4 - magnetometer information
5 - IMU calculated angles (roll, pitch, yaw)
6 - all IMU information

Returns a string.
";
DisconnectDevice::usage = "DisconnectDevice[ ]
Disconnects the device that is currently connect. Resets the connection global variable.
";


Begin["`Private`"];


ConnectDevice[dev_, baud_: 9600]:=
Module[{},
$startbit = 124;
Return[DeviceOpen["Serial", {dev, "BaudRate" -> baud}]];
];


WriteMessage[dev_, boardid_: 1, arg_: 0, msg_]:=
Module[{},
 numparam = Length[msg] + 1;
 len = numparam + 2;
 func = 0;

 (*Write protocol separately*)
 DeviceWrite[dev, $startbit];
 DeviceWrite[dev, boardid];
 DeviceWrite[dev, len];
 DeviceWrite[dev, func];
 DeviceWrite[dev, arg];

 (*Checksum*)
 sum = $startbit + boardid + len + func + arg;
 Do[
  DeviceWrite[dev, msg[[i]]];
  sum = sum + msg[[i]];
 , {i, 1, Length[msg]}]

 (*Make sure checksum is below 256 before writing*)
 While[sum > 255, sum = sum - 256;];
 DeviceWrite[dev, sum];
]


ReadMessage[dev_, boardid_: 1, arg_: 0]:=
Module[{},
 DeviceReadBuffer[dev];

 len = 3;
 func = 1;

 (*Write protocol separately*)
 DeviceWrite[dev, $startbit];
 DeviceWrite[dev, boardid];
 DeviceWrite[dev, len];
 DeviceWrite[dev, func];
 DeviceWrite[dev, arg];

 (*Checksum*)
 sum = $startbit + boardid + len + func + arg;
 While[sum > 255, sum = sum - 256;];
 DeviceWrite[dev, sum];

 (*Read character by character from serial until semicolon*)
 token = "1";
 reader = "";
 While[token!=";",
  token = FromCharacterCode[DeviceRead[dev]];
  If[token==";", Break;,reader = reader <> ToString[token];];
 ];
 Return[reader];
]


DisconnectDevice[dev_]:=DeviceClose[dev];


End[];
EndPackage[];
