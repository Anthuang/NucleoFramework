(* ::Package:: *)

BeginPackage["IMUFilter`"];


IMUInit::usage="Initializes values.";
IMUFilter::usage="Calculates Roll, Pitch, Yaw with input from IMU.";
GyroCalibrate::usage="Calibrates the Gyroscope by calculating the average.";
GetOffsets::usage="Returns the offsets.";


Begin["Private`"];


IMUInit[]:=Module[{},
$q0=1.0;$q1=0.0;$q2=0.0;$q3=0.0;
$exInt=0.0;$eyInt=0.0;$ezInt=0.0;
$gyroDataX={};$gyroOffsetX=0.0;$gyroDataY={};$gyroOffsetY=0.0;$gyroDataZ={};$gyroOffsetZ=0.0;
$acceDataX={};$acceOffsetX=0.0;$acceScaleX=0.0;
$acceDataY={};$acceOffsetY=0.0;$acceScaleY=0.0;
$acceDataZ={};$acceOffsetZ=0.0;$acceScaleZ=0.0;
];


IMUFilter[dt_,gx_,gy_,gz_,ax_,ay_,az_,mx_,my_,mz_]:=Module[{},
gxc=gx-$gyroOffsetX;
gyc=gy-$gyroOffsetY;
gzc=gz-$gyroOffsetZ;
axc=(ax-$acceOffsetX)*$acceScaleX;
ayc=(ay-$acceOffsetY)*$acceScaleY;
ayz=(az-$acceOffsetZ)*$acceScaleZ;

(*Gyro in radians per second*)
radgx=gxc*Pi/180;
radgy=gyc*Pi/180;
radgz=gzc*Pi/180;

q=AHRSUpdate[dt/2,radgx,radgy,radgz,axc,ayc,azc,mx,my,mz];
q0=q[[1]];q1=q[[2]];q2=q[[3]];q3=q[[4]];

rangle={0.0,0.0,0.0};
rangle[[1]]=ArcTan[1-2*(q1*q1+q2*q2),2*q0*q1+2*q2*q3];
rangle[[2]]=ArcSin[2*q0*q2-2*q3*q1];
rangle[[3]]=ArcTan[1-2*(q2*q2+q3*q3),2*q0*q3+2*q1*q2];

Return[{rangle[[1]]*180/Pi,rangle[[2]]*180/Pi,rangle[[3]]*180/Pi}];
];


AHRSUpdate[dt_,gx_,gy_,gz_,ax_,ay_,az_,mx_,my_,mz_]:=Module[{},
norm=0.0;
hx=0.0;hy=0.0;hz=0.0;bx=0.0;bz=0.0;
vx=0.0;vy=0.0;vz=0.0;wx=0.0;wy=0.0;wz=0.0;
Ki=0.005;Kp=2.0;

(*auxiliary variables to reduce number of repeated operations*)
q0q0=$q0*$q0;
q0q1=$q0*$q1;
q0q2=$q0*$q2;
q0q3=$q0*$q3;
q1q1=$q1*$q1;
q1q2=$q1*$q2;
q1q3=$q1*$q3;
q2q2=$q2*$q2;
q2q3=$q2*$q3;
q3q3=$q3*$q3;

(*normalise the measurements*)
norm=Sqrt[ax*ax+ay*ay+az*az];
If[norm==0.0,Return[{0.0,0.0,0.0,0.0}]];
normax=ax/norm;
normay=ay/norm;
normaz=az/norm;
norm=Sqrt[mx*mx+my*my+mz*mz];
If[norm==0.0,Return[{0.0,0.0,0.0,0.0}]];
normmx=mx/norm;
normmy=my/norm;
normmz=mz/norm;

(*compute reference direction of flux*)
hx=2*normmx*(0.5-q2q2-q3q3)+2*normmy*(q1q2-q0q3)+2*normmz*(q1q3+q0q2);
hy=2*normmx*(q1q2+q0q3)+2*normmy*(0.5-q1q1-q3q3)+2*normmz*(q2q3-q0q1);
hz=2*normmx*(q1q3-q0q2)+2*normmy*(q2q3+q0q1)+2*normmz*(0.5-q1q1-q2q2);
bx=Sqrt[(hx*hx)+(hy*hy)];
bz=hz;

(*estimated direction of gravity and flux (v and w)*)
vx=2*(q1q3-q0q2);
vy=2*(q0q1+q2q3);
vz=q0q0-q1q1-q2q2+q3q3;
wx=2*bx*(0.5-q2q2-q3q3)+2*bz*(q1q3-q0q2);
wy=2*bx*(q1q2-q0q3)+2*bz*(q0q1+q2q3);
wz=2*bx*(q0q2+q1q3)+2*bz*(0.5-q1q1-q2q2);

(*error is sum of cross product between reference direction of fields and direction measured by sensors*)
ex=(normay*vz-normaz*vy)+(normmy*wz-normmz*wy);
ey=(normaz*vx-normax*vz)+(normmz*wx-normmx*wz);
ez=(normax*vy-normay*vx)+(normmx*wy-normmy*wx);

(*integral error scaled integral gain*)
exInt2=$exInt+ex*Ki;
eyInt2=$eyInt+ey*Ki;
ezInt2=$ezInt+ez*Ki;
$exInt=exInt2;
$eyInt=eyInt2;
$ezInt=ezInt2;

(*adjusted gyroscope measurements*)
adjgx=gx+Kp*ex+$exInt;
adjgy=gy+Kp*ey+$eyInt;
adjgz=gz+Kp*ez+$ezInt;

(*integrate quaternion rate and normalise*)
q02=$q0+(-$q1*adjgx-$q2*adjgy-$q3*adjgz)*dt;
q12=$q1+(q02*adjgx+$q2*adjgz-$q3*adjgy)*dt;
q22=$q2+(q02*adjgy-q12*adjgz+$q3*adjgx)*dt;
q32=$q3+(q02*adjgz+q12*adjgy-q22*adjgx)*dt;

(*normalise quaternion*)
norm=Sqrt[q02*q02+q12*q12+q22*q22+q32*q32];
q03=q02/norm;
q13=q12/norm;
q23=q22/norm;
q33=q32/norm;
$q0=q03;
$q1=q13;
$q2=q23;
$q3=q33;

Return[{$q0,$q1,$q2,$q3}];
];


GyroCalibrate[gx_,gy_,gz_]:=Module[{},
AppendTo[$gyroDataX,gx];AppendTo[$gyroDataY,gy];AppendTo[$gyroDataZ,gz];
$gyroOffsetX=Total[$gyroDataX]/Length[$gyroDataX];
$gyroOffsetY=Total[$gyroDataY]/Length[$gyroDataY];
$gyroOffsetZ=Total[$gyroDataZ]/Length[$gyroDataZ];
];


AcceCalibrate[ax_,ay_,az_]:=Module[{},
AppendTo[$acceDataX,ax];AppendTo[$acceDataY,ay];AppendTo[$acceDataZ,az];
rangeX=Max[$acceDataX]-Min[$acceDataX];$acceOffsetX=rangeX/2;
rangeY=Max[$acceDataY]-Min[$acceDataY];$acceOffsetY=rangeY/2;
rangeZ=Max[$acceDataZ]-Min[$acceDataZ];$acceOffsetZ=rangeZ/2;
If[rangeX>rangeY&&rangeX>rangeZ,
$acceScaleX=rangeX/rangeX;
$acceScaleX=rangeX/rangeY;
$acceScaleX=rangeX/rangeZ;
];
If[rangeY>rangeX&&rangeY>rangeZ,
$acceScaleX=rangeY/rangeX;
$acceScaleX=rangeY/rangeY;
$acceScaleX=rangeY/rangeZ;
];
If[rangeZ>rangeX&&rangeZ>rangeY,
$acceScaleX=rangeZ/rangeX;
$acceScaleX=rangeZ/rangeY;
$acceScaleX=rangeZ/rangeZ;
];
]


GetOffsets[]:=Module[{},
Return[{$gyroOffsetX,$gyroOffsetY,$gyroOffsetZ}];
]


End[];
EndPackage[];
