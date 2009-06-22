// UDMv4.45 //
/***************************************************************/
var um={'menuClasses':[],'itemClasses':[],'menuCode':[]};
/***************************************************************\

  ULTIMATE DROP DOWN MENU Version 4.45 by Brothercake
  http://www.udm4.com/
  
  This script may not be used or distributed without license

\***************************************************************/


/***************************************************************\
 * CORE CONFIGURATION
\***************************************************************/


//path to images folder 
um.baseSRC = "udm_resources/";


//navbar orientation
um.orientation = [
	"horizontal",	// alignment ["vertical"|"horizontal"|"popup"|"expanding"]
	"left",		// h align ["left"|"right"|"rtl"]
	"top",		// v align ["top"|"bottom"]
	"relative",	// positioning ["relative"|"absolute"|"fixed"|"allfixed"]
	"0.5em",	// x position ["em"|"ex"|"px"|"0"]
	"0.5em",	// y position ["em"|"ex"|"px"|"0"]
	"1000",		// z order ["0" to "10000"] (menu takes 20000 headroom)
	];
	

//navbar list output
um.list = [
	"flexible",	// horizontal overflow ["rigid"|"flexible"]
	"yes",		// -SPARE-
	"no", 		// -SPARE-
	];


//menu behaviors	
um.behaviors = [
	"200",		// open timer ["milliseconds"|"0"]
	"500",		// close timer ["milliseconds"|"never"|"0"]
	"yes",		// reposition menus to stay inside the viewport ["yes"|"no"]
	"default",	// manage windowed controls for win/ie ["default","hide","iframe","none"]
	];


//reset behaviors
um.reset = [
	"yes",		// reset from document mouse click ["yes"|"no"]
	"yes",		// reset from window resize ["yes"|"no"]
	"yes",		// reset from text resize ["yes"|"no"]
	"no",		// reset after following link ["yes"|"no"]
	];


//horizontal continuation strip
um.hstrip = [
	"#225588;",		// background ["color"|"#hex"|"rgb()"|"image.gif"|"none"]
	"no",		// copy item margin-right to margin-bottom ["yes"|"no"]
	];
	
	
/***************************************************************\
 * MODULE SETTINGS
\***************************************************************/


//keyboard navigation
um.keys = [
	"38",		// up ["n"] ("38" = up arrow key)
	"39",		// right ["n"] ("39" = right arrow key)
	"40",		// down ["n"] ("40" = down arrow key)
	"37",		// left ["n"] ("37" = left arrow key)
	"123",		// hotkey ["n"] ("123" = F12)
	"none",		// hotkey modifier ["none"|"shiftKey"|"ctrlKey"|"altKey"|"metaKey"]
	"27",		// escape ["n"|"none"] ("27" = escape key)
	"document.getElementsByTagName('a')[4]", // exit focus ["js-expression"]
	];


/***************************************************************\
 * NAVBAR DEFAULT STYLES
\***************************************************************/


//styles which apply to the navbar
um.navbar = [
	"0",		// nav to menu x-offset (+-)["n" pixels]
	"1",	// nav to menu y-offset (+-)["n" pixels]
	"7.5em",	// width ["em"|"ex"|"px"] (vertical navbar only - horizontal navbar items have "auto" width) ("%" doesn't work right) 
	];


//styles which apply to each navbar item
um.items = [
	"0",		// margin between items ["n" pixels]
	"1",		// border size ["n" pixels] (single value only)
	"collapse",	// border collapse ["collapse"|"separate"] (only applies when margin = "0")
	"#225588",// border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"#225588",// hover/focus border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// hover/focus border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"#225588",// visited border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",// visited border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"10",		// left/right padding ["n" pixels] (single value only)
	"3",		// top/bottom padding ["n" pixels] (single value only)
	"#225588",// background ["color"|"#hex"|"rgb()"|"image.gif"]
	"#0067B3",// hover/focus background ["color"|"#hex"|"rgb()"|"image.gif"]
	"#225588",// visited background ["color"|"#hex"|"rgb()"|"image.gif"]
	"90%",		// font size ["em"|"ex"|"%"|"px"|"pt"|"absolute-size"|"relative-size"]
	"verdana,sans-serif",// font family ["font1,font2,font3"] (always end with a generic family name)
	"bold",		// font weight ["normal"|"bold"|"bolder"|"lighter|"100" to "900"]
	"none",		// text decoration ["none"|"underline"|"overline"|"line-through"]
	"left",		// text-align ["left"|"right"|"center"]
	"#FFFFFF",	// color ["color"|"#hex"|"rgb()"]
	"#FFFFFF",	// hover/focus color ["color"|"#hex"|"rgb()"]
	"#FFFFFF",	// visited color ["color"|"#hex"|"rgb()"]
	"normal",	// font-style ["normal"|"italic"|"oblique"]
	"normal",	// hover/focus font-style ["normal"|"italic"|"oblique"]
	"normal",	// visited font-style ["normal"|"italic"|"oblique"]
	"",// additional link CSS (careful!)
	"",// additional hover/focus CSS (careful!)
	"",// additional visited CSS (careful!)
	"none",// menu indicator character/image ["text"|"image.gif"|"none"] 
	"",// menu indicator rollover image ["image.gif"|"none"] (only when using image arrows)
	"",		// clipping width of indicator image ["n" pixels] (only when using image arrows)
	"",		// alt text of indicator image ["text"] (only when using image arrows)
	];


/***************************************************************\
 * MENU DEFAULT STYLES
\***************************************************************/


//styles which apply to each menu
um.menus = [
	"1",		// menu to menu x-offset (+-)["n" pixels]
	"-4",	// menu to menu y-offset (+-)["n" pixels]
	"0",		// border size ["n" pixels] (single value only) 
	"#225588",// border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"10em",	// width ["em"|"ex"|"px"]
	"2",		// padding ["n" pixels] (single value only) 
	"#225588",	// background ["color"|"#hex"|"rgb()"|"image.gif"]
	"",		// additional menu CSS (careful!) (you can use a transition here but *not* a static filter)
	"none",// shadow background ["color"|"#hex"|"rgb()"|"image.gif"|"none"]
	"0",		// shadow offset (+-)["em"|"ex"|"px"|"%"|"0"]
	"",// additional shadow layer CSS (if you use a Microsoft.Shadow filter here then Win/IE5.5+ will do that *instead* of default shadow)
	];


//styles which apply to each menu item
um.menuItems = [
	"0",		// margin around items ["n" pixels] (single value only; margins are like table cellspacing)
	"0",		// border size ["n" pixels] (single value only)
	"collapse",	// border collapse ["collapse"|"separate"] (only applies when margin = "0")
	"#225588",	// border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"#225588",		// hover/focus border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// hover/focus border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"#225588",	// visited border colors ["color"|"#hex"|"rgb()"] (single, double or four values)
	"solid",	// visited border styles ["solid"|"double"|"dotted"|"dashed"|"groove"|"ridge"|"inset"|"outset"] (single, double or four values; be careful with using "none")
	"5",		// left/right padding ["n" pixels] (single value only) 
	"1",		// top/bottom padding ["n" pixels] (single value only) 
	"#225588",	// background ["color"|"#hex"|"rgb()"|"image.gif"]
	"#0067B3",	// hover/focus background ["color"|"#hex"|"rgb()"|"image.gif"]
	"#225588",	// visited background ["color"|"#hex"|"rgb()"|"image.gif"]
	"80%",		// font size ["em"|"ex"|"%"|"px"|"pt"|"absolute-size"|"relative-size"]
	"verdana,sans-serif",// font family ["font1,font2,font3"] (always end with a generic family name)
	"normal",	// font weight ["normal"|"bold"|"bolder"|"lighter|"100" to "900"]
	"none",		// text decoration ["none"|"underline"|"overline"|"line-through"]
	"left",		// text-align ["left"|"right"|"center"]
	"#FFFFFF",		// color ["color"|"#hex"|"rgb()"]
	"#FFFFFF",		// hover/focus color ["color"|"#hex"|"rgb()"]
	"#FFFFFF",		// visited color ["color"|"#hex"|"rgb()"]
	"normal",	// font-style ["normal"|"italic"|"oblique"]
	"normal",	// hover/focus font-style ["normal"|"italic"|"oblique"]
	"normal",	// visited font-style ["normal"|"italic"|"oblique"]
	"",		// additional link CSS (careful!)
	"",		// additional hover/focus CSS (careful!)
	"",		// additional visited CSS (careful!)
	"none",		// submenu indicator character/image ["text"|"image.gif"|"none"] 
	"",		// submenu indicator rollover image ["image.gif"|"none"] (only when using image arrows)
	"",		// clipping width of indicator image ["n" pixels] (only when using image arrows)
	"",		// alt text of indicator image ["text"] (only when using image arrows)
	];


/***************************************************************\
 * MENU CLASSES
\***************************************************************/





/***************************************************************\
 * DYNAMIC MENUS
\***************************************************************/



	
	
/***************************************************************\
\***************************************************************/

// UDMv4.45 //
///////////////////////////////////////////////////////////////////
//                                                               //
//  ULTIMATE DROP DOWN MENU Version 4.45 by Brothercake          //
//  http://www.udm4.com/                                         //
//                                                               //
//  This script may not be used or distributed without license   //
//                                                               //
///////////////////////////////////////////////////////////////////
var umTree=null;um.ready=0;um.pi=function(n){n=parseInt(n,10);return (isNaN(n)?0:n);};um.un='undefined';um.m=document;um.gd=function(n){return um.m.getElementById(n);};um.xd=function(n){n.style.display='block';};um.xn=function(n){n.style.display='none';};um.xv=function(n){n.style.visibility='visible';};um.xh=function(n){n.style.visibility='hidden';};um.ne=function(n){return n.parentNode.className=='udm';};if(typeof um.reset==um.un){um.reset=['yes','yes','yes'];}if(typeof um.hstrip==um.un){um.hstrip=['none','yes'];}if(typeof um.reset[3]==um.un){um.reset[3]='no';}um.cx=['orientation','list','behaviors','navbar','items','menus','menuItems','menuClasses','itemClasses'];um.ei=0;um.e=[];um.v=[];um.w=[];um.vl=0;um.wl=0;um.ek=0;um.im=[];um.pcv=function(v){if(v&&/^[+\-]?[0-9]+$/.test(v)){v=um.pi(v);if((um.ei==10||um.ei==11)&&v<1){v=1;}}if(v&&/\.(gif|png|mng|jpg|jpeg|jpe|bmp)/i.test(v)){um.im[um.ek]=new Image;um.im[um.ek++].src=um.baseSRC+v;}return v;};um.d=(typeof um.m.getElementById!=um.un&&(typeof um.m.createElement!=um.un||typeof um.m.createElementNS!=um.un));um.u=navigator.userAgent.toLowerCase();um.o5=/opera[\/ ][56]/.test(um.u);um.k=(navigator.vendor=='KDE');if(um.o5){um.d=0;}um.b=(um.d||um.o5);um.o7=(um.d&&typeof window.opera!=um.un);um.o75=0;um.o73=0;um.o71=0;if(um.o7){um.ova=um.pi(um.u.split(/opera[\/ ]/)[1].match(/[7-9]/)[0]);um.ovi=um.pi(um.u.split(/opera[\/ ][7-9]\./)[1].match(/^[0-9]/)[0]);um.o75=(um.ova>=8||um.ovi>=5);um.o73=(um.ova>=8||um.ovi>=3);um.o71=(um.ova==7&&um.ovi<=1);}um.s=(navigator.vendor=='Apple Computer, Inc.');um.s2=(um.s&&typeof XMLHttpRequest!=um.un);um.wie=(um.d&&typeof um.m.all!=um.un&&typeof window.opera==um.un&&!um.k);um.mie=(um.wie&&um.u.indexOf('mac')>0);um.mx=(um.u.indexOf('tasman 0.9')>0);if(um.mx){um.mie=1;}um.omie=0;if(um.mie){um.wie=0;um.iev=um.u;um.iev=um.iev.split('msie ');um.iev[1]=um.iev[1].split(';');um.iev=parseFloat(um.iev[1][0],10);um.omie=(um.iev<5.2);}um.ie=(um.wie||um.mie);um.wie5=(um.wie&&um.u.indexOf('msie 5')>0);um.wie55=(um.wie&&um.u.indexOf('msie 5.5')>0);um.wie50=(um.wie5&&!um.wie55);um.wie6=(um.wie&&um.u.indexOf('msie 6')>0);if(um.wie6){um.wie55=1;}um.q=(um.wie5||(um.mie&&!um.mx)||((um.mx||um.wie6||um.o7)&&um.m.compatMode!='CSS1Compat'));um.og=0;um.dg=0;if(navigator.product=='Gecko'&&!um.s){um.sub=um.pi(navigator.productSub);um.og=(um.sub<20030312);um.dg=(um.sub<20030208);}if(um.b){var i=0;do{if(um.cx[i].indexOf('Classes')<0){um.cxl=um[um.cx[i]].length;var j=0;do{if(typeof um[um.cx[i]][j]!=um.un){um.pv=um.pcv(um[um.cx[i]][j]);um.e[um.ei]=um.pv;um.ei++;}j++;}while(j<um.cxl);}else{for(j in um[um.cx[i]]){if(typeof um[um.cx[i]][j]!='function'){um.cxl=um[um.cx[i]][j].length;var k=0;do{if(typeof um[um.cx[i]][j][k]!=um.un){um.pcv(um[um.cx[i]][j][k]);}k++;}while(k<um.cxl);if(um.cx[i]=='menuClasses'){um.v[j]=um[um.cx[i]][j];um.vl++;}else{um.w[j]=um[um.cx[i]][j];um.wl++;}}}}i++;}while(i<9);um.kb=(typeof um.keys!=um.un&&!(um.mie||um.o7||um.k||(um.s&&!um.s2)));um.skb=(um.kb||(typeof um.keys!=um.un&&((um.o7&&!um.o71)||um.k)));um.sp=(typeof um.speech!=um.un&&um.wie);if(typeof um.speech!=um.un){um.e[12]='no';um.e[0]='vertical';}um.rp=(um.e[3]=='relative');if(um.mx||(um.wie50&&um.rp)){um.e[12]='no';}um.dir='left';if(um.e[1]=='rtl'){um.dir='right';um.e[1]='right';}um.e[13]=(um.e[13]=='yes')?'default':(um.e[13]=='no')?'iframe':um.e[13];um.hz=(um.wie50&&um.e[13]=='default')||(um.wie&&um.e[13]=='hide');um.h=um.e[0]=='horizontal';i=4;do{if(parseFloat(um.e[i],10)<0){um.e[i]='0';}i++}while(i<6);if(um.h&&um.dir=='right'){um.e[4]='-'+um.e[4];}um.p=um.e[0]=='popup';if(um.p){um.va=['left','top','absolute','-2000px','-2000px'];i=0;do{um.e[i+1]=um.va[i];i++}while(i<5);um.e[14]=0;um.e[15]=0;}um.ep=0;if(um.e[0]=='expanding'){um.ep=1;um.e[0]='vertical';}um.a=(um.e[1]=='right');um.rg=(um.h&&um.e[7]=='rigid'&&um.dir!='right');um.fe=false;if(um.e[3]=='allfixed'){um.e[3]='fixed';if(um.wie){um.fe=true;}}um.f=(um.e[3]=='fixed'&&!(um.ie||um.og));um.nc=(um.e[17]==0&&um.e[19]=='collapse');um.mc=(um.e[61]==0&&um.e[63]=='collapse');um.nm=((um.og&&um.rp)||(um.omie&&um.h)||((um.dg||um.wie50)&&um.dir=='right'));um.nr=(um.nm||um.mie);um.ns=(um.dg||um.o71||(um.wie50&&um.rp)||(um.o7&&um.f)||um.mie);um.cns=(typeof um.m.createElementNS!=um.un);um.ss=(um.cns&&typeof um.m.styleSheets!=um.un&&!(um.s||um.k||um.mx));if(um.kb){i=0;do{um.keys[i]=um.pi(um.keys[i]);i++}while(i<5);if(um.keys[6]!='none'){um.keys[6]=um.pi(um.keys[6]);}else{um.keys[6]=-1;}}um.ni=/(gif|png|mng|jpg|jpeg|jpe|bmp)/i.test(um.e[45]);um.mi=/(gif|png|mng|jpg|jpeg|jpe|bmp)/i.test(um.e[89]);}um.rn=0;um.rv=[];um.addReceiver=function(f,c){um.rv[um.rn++]=[f,c];};um.gp=function(n){return n?um.vn(n.nodeName).toLowerCase()=='li'?n:this.gp(n.parentNode):null;};um.createElement=function(n,a){um.el=(um.cns)?um.m.createElementNS('http://www.w3.org/1999/xhtml',n):um.m.createElement(n);if(typeof a!=um.un){for(var i in a){switch(i){case 'text' :um.el.appendChild(um.m.createTextNode(a[i]));break;case 'class' :um.el.className=a[i];break;default :um.el.setAttribute(i,'');um.el[i]=a[i];break;}}}return um.el;};// UDMv4.45 //
///////////////////////////////////////////////////////////////////
//                                                               //
//  ULTIMATE DROP DOWN MENU Version 4.45 by Brothercake          //
//  http://www.udm4.com/                                         //
//                                                               //
//  This script may not be used or distributed without license   //
//                                                               //
///////////////////////////////////////////////////////////////////
if(um.b){um.bk=function(udmP){return (/(gif|png|mng|jpg|jpeg|jpe|bmp)/i.test(udmP))?'background-image:url('+um.baseSRC+udmP+');':(udmP=='none')?'':um.t[33]+'background-color:'+udmP+';';};um.t=['margin-left:','padding-top:','@media screen,projection{','margin-top:0;','padding-left:','border-width:','border-color:','border-style:','margin-left:0;','display:none;','margin-right:','text-decoration:','position:absolute;','margin-bottom:','visibility:hidden;','cursor:default !important;','position:static;','display:block;','@media Screen,Projection{','position:relative;','* html .udm ul ',' a:hover .udmA',' a:focus .udmA',' a:visited .udmA','',' a:visited:hover',' a.nohref:focus',' a.nohref:hover','width:auto;height:auto;','cursor:pointer !important;','background-repeat:no-repeat;background-position:','.udm:not(:nth-child(n))',' a.nohref .udmA','background-image:none;','* html .udm li a',' a.udmR:visited',' a.udmR .udmA',' a.udmY:visited',' a.udmY .udmA','display:block;visibility:visible;height:0;','overflow:scroll;','overflow:visible;'];var j=0;um.r=[];um.ad=(um.a)?'left':'right';um.dra=(um.dir=='right');um.r[j++]='.udm,.udm li,.udm ul{margin:0;padding:0;list-style-type:none;}';if(um.dra){if(um.h&&um.rp){um.r[j++]='* html .udm{left:100%;left:expression(this.offsetWidth);left/**/:0 !important;}';}um.r[j++]='.udm,.udm li,.udm ul{unicode-bidi:bidi-override;direction:ltr;}';um.r[j++]='.udm a *,.udm a {unicode-bidi/**/:bidi-override;direction/**/:rtl;}';}um.na=(um.h)?'left':um.e[1];um.txl=(um.h)?'left':um.e[35];um.r[j++]='.udm{position:'+um.e[3]+';'+um.na+':0;'+um.e[2]+':0;z-index:'+(um.e[6]+19000)+';width:'+um.e[16]+';'+um.t[15]+'border:none;text-align:left;}';if(um.e[3]=='fixed'){um.r[j++]='* html .udm{'+um.t[12]+'}';um.r[j++]='ul[id="udm"]{'+um.t[12]+'}';um.r[j++]='ul/**/[id="udm"]{position:fixed;}';}if(um.h){um.hfl=(um.hstrip[0]=='none')?'none':um.dir;um.r[j++]='.udm{'+um.bk(um.hstrip[0])+'float:'+um.hfl+';width:100%;}';if(um.hstrip[0]!='none'){um.r[j++]='ul[class="udm"]{float:none;}';um.r[j++]='ul/**/[class="udm"]{float:'+um.dir+';}';um.r[j++]='.udm{margin-'+um.e[2]+':0;'+um.e[2]+':'+um.e[5]+';}';um.r[j++]=um.t[2]+'.udm{margin-'+um.e[2]+':'+um.e[5]+';'+um.e[2]+':0}}';um.r[j++]=um.t[2]+um.t[31]+'{margin-'+um.e[2]+':0;'+um.e[2]+':'+um.e[5]+';}}';}else{um.r[j++]=um.t[2]+'.udm{float:'+um.dir+';}}';um.r[j++]=um.t[2]+um.t[31]+'{float:none;}}';if(um.rp){um.r[j++]='.udm{padding-'+um.e[2]+':'+um.e[5]+';}';}else{um.r[j++]='.udm{margin-'+um.e[2]+':'+um.e[5]+';}';}}if(um.dra){um.r[j++]='.udm>li:first-child{margin-right:'+um.e[4].replace('-','')+';}';}else{um.r[j++]='.udm>li:first-child{'+um.t[0]+um.e[4]+';}';}um.r[j++]=um.t[18]+'.udm>li:first-child{'+um.t[8]+'margin-right:0;}}';um.r[j++]=um.t[2]+um.t[31]+'>li:first-child{'+um.t[0]+um.e[4]+';}}';um.r[j++]='.udm li{left:'+um.e[4]+';}';um.r[j++]=um.t[2]+'.udm li{'+um.t[19]+'}}';um.r[j++]='.udm ul li{left:0;}';um.r[j++]='ul[class^="udm"] li{left:0;'+um.t[16]+'}';um.r[j++]=um.t[18]+'ul[class^="udm"] li{left:'+um.e[4]+';'+um.t[19]+'}}';um.r[j++]=um.t[2]+um.t[31]+' li{'+um.t[16]+'}}';um.r[j++]=um.t[18]+'.udm/**/[class="udm"] ul li{'+um.t[19]+'left:0;}}';um.r[j++]=um.t[2]+'.udm ul li:not(:nth-child(n)){'+um.t[16]+'}}';um.r[j++]='.udm li{'+um.t[17]+'width:auto;float:'+um.dir+';}';um.r[j++]='.udm li a{'+um.t[16]+um.t[17]+'float:'+um.dir+';white-space:nowrap;}';um.r[j++]=um.t[2]+'.udm l\\i a{'+um.t[19]+'float:none;}}';um.r[j++]='ul[class^="udm"] li a{'+um.t[19]+'float:none;}';um.r[j++]=um.t[2]+um.t[34]+'{'+um.t[19]+'float:none;}}';um.r[j++]=um.t[2]+'.udm li a:not(:nth-child(n)){'+um.t[16]+'float:'+um.dir+';}}';if(um.dra){um.r[j++]=um.t[2]+um.t[34]+'{'+um.t[16]+'}}';}um.r[j++]='.udm ul li a{'+um.t[19]+'float:none !important;white-space:normal;}';if(um.nc){um.r[j++]='.udm li a{'+um.t[0]+'-'+um.e[18]+'px;}';um.r[j++]=um.t[18]+'.udm li{'+um.t[0]+'-'+um.e[18]+'px !important;}}';um.r[j++]=um.t[18]+'.udm li a{'+um.t[8]+'}}';um.r[j++]=um.t[2]+um.t[31]+' li:first-child{'+um.t[0]+um.e[4]+' !important;}}';um.r[j++]=um.t[2]+um.t[31]+' li:first-child a{'+um.t[0]+'-'+um.e[18]+'px;}}';um.r[j++]=um.t[2]+um.t[31]+' ul li:first-child{'+um.t[0]+'0 !important;}}';um.r[j++]='head:first-child+body ul[class^="udm"] li:not(:first-child){'+um.t[0]+'-'+um.e[18]+'px;}';um.r[j++]='.udm ul li{'+um.t[0]+'0 !important;}';um.r[j++]='ul[class^="udm"] ul li{'+um.t[0]+'0 !important;}';}else{um.r[j++]='.udm li,.udm li:first-child{'+um.t[10]+um.e[17]+'px;}';if(um.dra){um.r[j++]='* html .udm li{'+um.t[10]+'0;'+um.t[0]+um.e[17]+'px;}';}um.r[j++]='.udm ul li{'+um.t[8]+um.t[10]+'0;}';if(um.hstrip[1]=='yes'){um.r[j++]='.udm li a{'+um.t[13]+um.e[17]+'px;}';um.r[j++]='.udm ul li a{'+um.t[13]+'0;}';um.r[j++]='ul[class^="udm"] li a{'+um.t[13]+'0;}';um.r[j++]='ul[class^="udm"] li{'+um.t[13]+um.e[17]+'px;}';um.r[j++]='ul[class^="udm"] ul li{'+um.t[13]+'0;}';}}}else{if(um.rp){um.r[j++]='.udm{'+um.t[16]+'padding-'+um.e[1]+':'+um.e[4]+';padding-'+um.e[2]+':'+um.e[5]+';}';}else{um.r[j++]='.udm{margin-'+um.e[1]+':'+um.e[4]+';margin-'+um.e[2]+':'+um.e[5]+';}';}um.ps=(um.p)?'absolute':'static';um.r[j++]='.udm li{'+um.t[17]+'width:'+um.e[16]+';position:'+um.ps+';}';um.ps=(um.p)?'static':'relative';um.r[j++]=um.t[18]+'.udm/**/[class="udm"] li{position:'+um.ps+';}}';um.r[j++]=um.t[18]+'.udm/**/[class="udm"] ul li{'+um.t[19]+'}}';um.r[j++]=um.t[2]+'.udm li:not(:nth-child(n)),.udm ul li:not(:nth-child(n)){'+um.t[16]+'}}';um.r[j++]='.udm li a{'+um.t[19]+um.t[17]+'}';if(um.nc){um.r[j++]='.udm a{margin-top:-'+um.e[18]+'px;}';}else{um.r[j++]='.udm li{'+um.t[13]+um.e[17]+'px;}';um.r[j++]='.udm ul li{'+um.t[13]+'0;}';}}um.r[j++]='.udm ul a{margin:0;}';if(um.mc){um.r[j++]='.udm ul li{margin-top:-'+um.e[62]+'px;}';um.r[j++]='.udm ul li:first-child{margin-top:0px;}';}else{um.r[j++]='.udm ul li{'+um.t[13]+um.e[61]+'px !important;}';um.r[j++]='.udm ul li:first-child{margin-top:'+um.e[61]+'px;}';um.r[j++]='.udm ul a{margin-top:0;margin-right:'+um.e[61]+'px !important;margin-bottom:0;'+um.t[0]+um.e[61]+'px !important;}';}um.r[j++]='.udm ul{'+um.bk(um.e[56])+um.t[15]+'width:'+um.e[54]+';height:auto;'+um.t[5]+um.e[51]+'px;'+um.t[6]+um.e[52]+';'+um.t[7]+um.e[53]+';'+um.t[12]+'z-index:'+(um.e[6]+19100)+';padding:'+um.e[55]+'px;'+um.e[57]+'}';um.r[j++]='.udm ul li{'+um.t[15]+'width:100%;'+um.t[16]+'float:none;}';um.r[j++]='.udm ul{'+um.t[9]+um.t[14]+'}';um.r[j++]='html/**/[xmlns] .udm u\\l{'+um.t[39]+um.t[40]+'left:-10000px;}';um.r[j++]=um.t[2]+um.t[20]+'{'+um.t[39]+um.t[40]+'top:-10000px;}}';um.r[j++]='ul.udm/**/[class^="udm"] u\\l{'+um.t[39]+um.t[41]+'left:-1000em;}';um.r[j++]=um.t[2]+'* html .udm:not(:nth-child(n)) ul{'+um.t[9]+um.t[14]+'left:auto;top:auto;}}';if(um.e[45]!='none'||um.e[89]!='none'){um.r[j++]='.udm a .udmA{visibility:hidden;margin:0 '+um.e[26]+'px;'+um.t[17]+um.t[29]+um.t[12]+um.ad+':0;top:0;text-align:'+um.ad+';border:none;cursor:inherit !important;}';um.r[j++]='.udm a .udmA img{display:block;}';um.r[j++]='.udm ul a .udmA{margin:0 '+um.e[70]+'px;}';if(um.a){um.r[j++]='* html .udm '+((um.h)?'ul ':'')+'a{height:1%;}';if(um.h&&um.dir!='right'){um.r[j++]='.udm a/**/ {width:expression("auto",this.runtimeStyle.width=(!document.compatMode||compatMode=="BackCompat")?"100%":(this.parentNode.offsetWidth-(isNaN(parseInt(this.currentStyle.marginRight))?0:parseInt(this.currentStyle.marginRight))-(isNaN(parseInt(this.currentStyle.marginLeft))?0:parseInt(this.currentStyle.marginLeft))-(isNaN(parseInt(this.currentStyle.paddingRight))?0:parseInt(this.currentStyle.paddingRight))-(isNaN(parseInt(this.currentStyle.paddingLeft))?0:parseInt(this.currentStyle.paddingLeft))-(isNaN(parseInt(this.currentStyle.borderRightWidth))?0:parseInt(this.currentStyle.borderRightWidth))-(isNaN(parseInt(this.currentStyle.borderLeftWidth))?0:parseInt(this.currentStyle.borderLeftWidth))));}';um.r[j++]='.udm ul a{width:auto;}';}}else{um.r[j++]='* html .udm a .udmA{'+um.ad+':'+um.e[18]+'px;top:'+um.e[18]+'px;}';um.r[j++]=um.t[20]+'a .udmA{'+um.ad+':'+(um.e[62]+um.e[61])+'px;top:'+um.e[62]+'px;}';}}if(um.e[58]!='none'){um.r[j++]='.udm .udmS{'+um.t[0]+um.e[59]+';margin-top:'+(um.e[59]==0?0:um.e[59].replace('-',''))+';}';um.r[j++]='.udm .udmS{'+um.bk(um.e[58])+um.t[15]+um.t[12]+'z-index:'+(um.e[6]+19050)+';'+um.t[28]+'left:0px;top:0px;'+um.t[9]+um.e[60]+'}';if(/filter\:progid\:DXImageTransform\.Microsoft\.Shadow/.test(um.e[60])){um.r[j++]=um.t[2]+'* html .udm .udmS/**/ {'+um.t[33]+'background:#ccc;'+um.t[8]+um.t[3]+'}}';}}um.r[j++]='.udm a,.udm a:link,.udm a.nohref{'+um.bk(um.e[28])+um.t[29]+'z-index:'+um.e[6]+';text-align:'+um.txl+';'+um.t[7]+um.e[21]+';'+um.t[6]+um.e[20]+';'+um.t[4]+um.e[26]+'px;padding-right:'+um.e[26]+'px;'+um.t[1]+um.e[27]+'px !important;padding-bottom:'+um.e[27]+'px !important;'+um.t[11]+um.e[34]+';color:'+um.e[36]+';'+um.t[5]+um.e[18]+'px;font-style:'+um.e[39]+';font-family:'+um.e[32]+';font-weight:'+um.e[33]+' !important;}';um.r[j++]='.udm a,.udm a.nohref{font-size:'+um.e[31]+';}';if(um.e[45]!='none'||um.e[89]!='none'){um.r[j++]='.udm a .udmA,.udm a:link .udmA,.udm'+um.t[32]+'{font-family:'+um.e[32]+';font-weight:'+um.e[33]+' !important;}';}if(um.e[42]!=''){um.r[j++]='.udm li a,.udm li a:link,.udm li a.nohref,.udm li a:visited{'+um.e[42]+'}';}um.r[j++]='.udm li a:visited{'+um.bk(um.e[30])+um.t[5]+um.e[18]+'px;color:'+um.e[38]+';font-style:'+um.e[41]+';'+um.t[7]+um.e[25]+';'+um.t[6]+um.e[24]+';'+um.e[44]+'}';um.r[j++]='.udm li a.udmR,.udm li a.udmY,.udm li'+um.t[35]+',.udm li'+um.t[37]+',.udm li a:hover,.udm li a:focus,.udm li'+um.t[27]+',.udm li'+um.t[26]+'{font-style:'+um.e[40]+';'+um.bk(um.e[29])+um.t[11]+um.e[34]+';color:'+um.e[37]+';'+um.t[6]+um.e[22]+';'+um.t[7]+um.e[23]+';'+um.t[5]+um.e[18]+'px;'+um.e[43]+'}';um.r[j++]='* html .udm li a:active{font-style:'+um.e[40]+';'+um.bk(um.e[29])+um.t[11]+um.e[34]+';color:'+um.e[37]+';'+um.t[6]+um.e[22]+';'+um.t[7]+um.e[23]+';'+um.t[5]+um.e[18]+'px;'+um.e[43]+'}';um.r[j++]='.udm ul a,.udm ul a:link,.udm ul a.nohref{'+um.bk(um.e[72])+'text-align:'+um.e[79]+';'+um.t[5]+um.e[62]+'px;'+um.t[7]+um.e[65]+';'+um.t[6]+um.e[64]+';'+um.t[4]+um.e[70]+'px;padding-right:'+um.e[70]+'px;'+um.t[1]+um.e[71]+'px !important;padding-bottom:'+um.e[71]+'px !important;'+um.t[11]+um.e[78]+';color:'+um.e[80]+';font-style:'+um.e[83]+';font-size:'+um.e[75]+';font-family:'+um.e[76]+';font-weight:'+um.e[77]+' !important;}';if(um.e[89]!='none'){um.r[j++]='.udm ul a .udmA,.udm ul a:link .udmA,.udm ul'+um.t[32]+'{font-family:'+um.e[76]+';font-weight:'+um.e[77]+' !important;}';}if(um.e[86]!=''){um.r[j++]='.udm ul li a,.udm ul li a:link,.udm ul li a.nohref,.udm ul li a:visited{'+um.e[86]+'}';}um.r[j++]='.udm ul li a:visited,'+um.t[20]+'li a:visited{'+um.bk(um.e[74])+'color:'+um.e[82]+';font-style:'+um.e[85]+';'+um.t[5]+um.e[62]+'px;'+um.t[7]+um.e[69]+';'+um.t[6]+um.e[68]+';'+um.e[88]+'}';um.r[j++]='.udm ul li a.udmR,.udm ul li a.udmY,.udm ul li'+um.t[35]+',.udm ul li'+um.t[37]+',.udm ul li a:hover,.udm ul li a:focus,.udm ul li'+um.t[27]+',.udm ul li'+um.t[26]+',.udm ul li'+um.t[25]+'{font-style:'+um.e[84]+';'+um.bk(um.e[73])+um.t[11]+um.e[78]+';color:'+um.e[81]+';'+um.t[6]+um.e[66]+';'+um.t[7]+um.e[67]+';'+um.t[5]+um.e[62]+'px;'+um.e[87]+'}';um.r[j++]='* html .udm ul li a:active{font-style:'+um.e[84]+';'+um.bk(um.e[73])+um.t[11]+um.e[78]+';color:'+um.e[81]+';'+um.t[6]+um.e[66]+';'+um.t[7]+um.e[67]+';'+um.t[5]+um.e[62]+'px;'+um.e[87]+'}';um.r[j++]='.udm a.nohref,.udm ul a.nohref{cursor:default !important;}';um.r[j++]='.udm h1,.udm h2,.udm h3,.udm h4,.udm h5,.udm h6{display:block;background:none;margin:0;padding:0;border:none;font-size:1em;font-weight:normal;text-decoration:none;}';if(um.h){um.r[j++]='.udm h3,.udm h4,.udm h5,.udm h6{display:inline;}';um.r[j++]='.udm h\\3,.udm h\\4,.udm h\\5,.udm h\\6{display:block;}';um.r[j++]='ul[class^="udm"] h3,ul[class^="udm"] h4,ul[class^="udm"] h5,ul[class^="udm"] h6{display:block;}';um.r[j++]='* html .udm h3,* html .udm h4,* html .udm h5,* html .udm h6{display:block;}';um.r[j++]=um.t[2]+'.udm h3,.udm h4,.udm h5,.udm h6{width:expression("auto",this.runtimeStyle.width=this.parentNode.offsetWidth);width/**/:auto;}}';um.r[j++]=um.t[2]+'.udm ul h3,.udm ul h4,.udm ul h5,.udm ul h6{width:expression("auto",this.runtimeStyle.width=this.parentNode.currentStyle.width);width/**/:auto;}}';}else{um.r[j++]='.udm h1,.udm h2,.udm h3,.udm h4,.udm h5,.udm h6{width:100%;}';}um.r[j++]=um.t[2]+'* html .udm li{display:inline;}}';um.floats=(um.h)?um.dir:um.e[1];um.r[j++]=um.t[2]+'* html .udm li,* html .udm ul li{display/**/:block;float/**/:'+um.floats+';}}';um.r[j++]=um.t[2]+um.t[31]+' li,'+um.t[31]+' ul li{display:block;float:'+um.floats+';}}';if(um.h){um.r[j++]=um.t[2]+'* html .udm li,'+um.t[20]+'li{clear:none;}}';}if(um.e[13]=='default'||um.e[13]=='hide'){um.r[j++]='select{visibility:visible;}';}if(um.e[13]=='default'||um.e[13]=='iframe'){um.r[j++]='.udm .udmC{'+um.t[12]+'left:0;top:0;z-index:'+(um.e[6]+19020)+';'+um.t[28]+'filter:alpha(opacity=0);}';}if(um.vl>0){for(i in um.v){if(typeof um.v[i]!='function'){um.r[j++]='.udm ul.'+i+'{width:'+um.v[i][2]+';'+um.t[6]+um.v[i][0]+';'+um.t[7]+um.v[i][1]+';'+um.bk(um.v[i][3])+um.v[i][4]+'}';um.r[j++]='.udm span.'+i+'{'+um.t[0]+um.v[i][6]+';margin-top:'+um.v[i][6].replace('-','')+';}';if(/filter\:progid\:DXImageTransform\.Microsoft\.Shadow/.test(um.v[i][7])){um.r[j++]=um.t[2]+'* html .udm span.'+i+'/**/ {'+um.t[8]+um.t[3]+'}}';}if(um.v[i][5]!='none'){um.r[j++]='.udm span.'+i+'{'+um.bk(um.v[i][5])+'filter:none;'+um.v[i][7]+'}';}}}}if(um.wl>0){for(i in um.w){if(typeof um.w[i]!='function'){um.bg=um.bk(um.w[i][6]);um.r[j++]='.udm li.'+i+' a,.udm li.'+i+' a:link,.udm li.'+i+' a.nohref{'+um.t[6]+um.w[i][0]+';'+um.t[7]+um.w[i][1]+';'+um.t[5]+um.e[62]+'px;'+um.bg+um.t[11]+um.w[i][12]+';text-align:'+um.w[i][13]+';color:'+um.w[i][14]+';font-style:'+um.w[i][17]+';font-size:'+um.w[i][9]+';}';um.r[j++]='.udm li.'+i+' a,.udm li.'+i+' a:link,.udm li.'+i+um.t[32]+',.udm li.'+i+' a,.udm li.'+i+um.t[32]+'{font-family:'+um.w[i][10]+';font-weight:'+um.w[i][11]+' !important;}';if(um.w[i][20]!=''){um.r[j++]='.udm ul li.'+i+' a,.udm ul li.'+i+' a:link,.udm ul li.'+i+' a.nohref,.udm ul li.'+i+' a:visited{'+um.w[i][20]+'}';}um.r[j++]='.udm ul li.'+i+' a:visited,'+um.t[20]+'li.'+i+' a:visited{'+um.bk(um.w[i][8])+'color:'+um.w[i][16]+';font-style:'+um.w[i][19]+';'+um.t[5]+um.e[62]+'px;'+um.t[6]+um.w[i][4]+';'+um.t[7]+um.w[i][5]+';'+um.w[i][22]+'}';um.r[j++]='.udm ul li.'+i+' a.udmR,.udm ul li.'+i+' a.udmY,.udm ul li.'+i+um.t[35]+',.udm ul li.'+i+um.t[37]+',.udm ul li.'+i+' a:hover,.udm ul li.'+i+' a:focus,.udm ul li.'+i+um.t[27]+',.udm ul li.'+i+um.t[26]+',.udm ul li.'+i+um.t[25]+'{'+um.bk(um.w[i][7])+um.t[11]+um.w[i][12]+';color:'+um.w[i][15]+';'+um.t[5]+um.e[62]+'px;'+um.t[6]+um.w[i][2]+';'+um.t[7]+um.w[i][3]+';font-style:'+um.w[i][18]+';'+um.w[i][21]+'}';um.r[j++]='* html .udm li.'+i+' a:active{'+um.bk(um.w[i][7])+um.t[11]+um.w[i][12]+';color:'+um.w[i][15]+';'+um.t[5]+um.e[62]+'px;'+um.t[6]+um.w[i][2]+';'+um.t[7]+um.w[i][3]+';font-style:'+um.w[i][18]+';'+um.w[i][21]+'}';}}}um.rLen=um.r.length;if(um.ss||um.o73){um.at={'type':'text/css','media':'screen,projection'};um.stn=um.createElement('html:style',um.at);document.getElementsByTagName('head')[0].appendChild(um.stn);if(um.ss){if(document.styleSheets.length==0){um.ss=false;}else{um.sy=document.styleSheets.item(document.styleSheets.length-1);i=0;do{um.sy.insertRule(um.r[i++],um.sy.cssRules.length);}while(i<um.rLen);}}else if(um.o73){i=0;do{um.stn.appendChild(document.createTextNode(um.r[i++]));}while(i<um.rLen);}}if(!(um.ss||um.o73)){um.styStr='';i=0;do{um.styStr+=um.r[i++];}while(i<um.rLen);document.write('<style type="text/css" media="screen,projection">'+um.styStr+'</style>');}}