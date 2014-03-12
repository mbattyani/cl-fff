
function f85425(Tabs, Panes, nTab, BaseClassName, SelectedClassName, remove){
    if (!nTab)
        nTab = 0;
    else
        nTab = parseInt(nTab);
    var Tab;
    for (var i = 0; i < Tabs.length; i++){
        Tab = Tabs[i];
        Tab.style.borderLeftStyle = "";
        Tab.style.borderRightStyle = "";
        Tab.className = BaseClassName;
        Panes[i].style.display = "none";
    }
    Tabs[nTab].className = SelectedClassName;
    Panes[nTab].style.display = "";
    if (remove){
        Tab = Tabs[nTab+1];
        if (Tab)
            Tab.style.borderLeftStyle = "none";
        Tab = Tabs[nTab-1];
        if (Tab)
            Tab.style.borderRightStyle = "none";
    }
}

function f8532(Tabs, table, SelClass)
{
  var iTabSelected = 0;
  var iLength = Tabs.length;
  for (var i = 0; i < iLength; i++)
      if (Tabs[i].className == SelClass) iTabSelected = i;
  table.setAttribute("s", iTabSelected);
}

function f826sv(name, val)
{
  var item;
  item=document.getElementById(name);
  if (item) item.value = val;
}

function f826si(name, val)
{
  var item;
  item=document.getElementById(name+'d');
  if (item) item.innerHTML = val;
}

function f826svi(name, val)
{
  f826sv(name, val);
  var item;
  item=document.getElementById(name+'d');
  if (item) item.innerHTML = val;
}

function f825s(name)
{
  var item;
  item=document.getElementById(name);
  if (item) item.style.display = "";
}

function f825h(name)
{
  var item;
  item=document.getElementById(name);
  if (item) item.style.display = "none";
}

function f8252s(name) {
    f825h(name+'d');
    f825s(name);
}

function f8252h(name) {f825h(name);f825s(name+'d');}

function f825foc(name)
{
  f825h(name);
  f825s(name+'d');
  Fck(name, '0');
}

var F3TimeOutCounter=3651;
var F3WatchdogCounter=3651;
var F3PullURL='';
// var F3PushURL;
var v685='';
var F3PacketSync=0;
var FrameNb = 12; var F3LinkId; var v690;


/*
function F3SetPushURL(v856){
// sets push url
    F3PushURL=v856;
};
*/

function F3SetSyncPacket(v856){
    F3PacketSync=v856;
};

function F3CheckSyncPacket(v856){
    return F3PacketSync<v856;
};



function f854(name, id){
    var s = "";
    for (var i = 0; i < 25; i++){
        var item = document.getElementById(name+'C'+i);
        if (item && item.checked)
	    s=s+'t';
        else
	    s=s+'n';
    }
    return id+'='+s;
}

function open1(url, dx, dy, item)
{
 v690 = window.open(url+'?link='+F3LinkId+'&item='+item,'pop','status=no,width='+dx+'px,height='+dy+'px,resizable=yes,scrollbars=yes');
};


function set_src(name, url, item) {
    document.getElementById(name).setAttribute("src", url+'?link='+F3LinkId+'&item='+item);
}

function make_src(url, item) {
    return url+'?link='+F3LinkId+'&item='+item;
}

function close()
{
 if (v690)
  {
   v690.close();
   v690 = null;
  }
};

function SendEvent(msg_type,name,value){
    var msg;
    var msg_stringify;
    // first send ID to get http-link, this should be fixed! get id form client
    msg = new Object();
    msg.type = "id";
    msg.value = F3LinkId;
    msg_stringify = JSON.stringify(msg);
    // alert ("ws.sends " + msg_stringify);
    ws.send(msg_stringify);

    // then send command {4,}
    msg.type = msg_type;
    msg.value = [name, value];
    msg_stringify = JSON.stringify(msg);
    // alert ("ws.sends " + msg_stringify);
    ws.send(msg_stringify);
}

function Fch(name, val){
    // '4'
    SendEvent('4',name,''+val);
    // SendToLink('4', Name, ''+val, F3PushURL);
};

function Fad(name, val){
    SendEvent('12',name,''+val);
    // SendToLink('12', Name, ''+val, F3PushURL);
};

function Fck(name, val){
    SendEvent('8',name,''+val);
    // SendToLink('8', Name, ''+val, F3PushURL);
};

function Fcl(name, val){
    SendEvent('13',name,''+val);
    // SendToLink('13', Name, ''+val, F3PushURL);
};

var ws = new WebSocket("ws://"+ window.location.hostname + ":1339/");

function send_id(){
    var msg;
    msg = new Object();
    msg.type = "id";
    msg.value = F3LinkId;
    ws.send(JSON.stringify(msg));
};



ws.onopen = function(){
    if(F3LinkId){
        console.log("ID: " + F3LinkId);
        send_id();
    }
};

 ws.onmessage = function (evt){
     console.log(evt.data);
     var data = JSON.parse(evt.data);
     if(data["action"]==="eval")
     {
         eval(data["data"]);
     }
     else {
         f826svi(data["itemname"], data["itemvalue"]);
     };
 };

ws.onerror = function(evt){
    window.evt = evt;
    alert("error:"+evt);
};


function F3SetPullURL(v856, LinkId){
    // sets pull  url
    F3PullURL=v856; // sets url-pull
    F3LinkId = LinkId; // sets interface-id
    if(ws.readyState===1){
        console.log("in F3SetPullURL, ID: " + F3LinkId +", state: "+ ws.readyState);
        send_id();
    }
};


function open2(text, dx, dy){ // What is this?
    var v690 = window.open('about:blank','pop','status=no,width='+dx+'px,height='+dy+'px,resizable=yes,scrollbars=yes');
    var v691 = v690.document;
    v691.open("text/html", "replace");
    v691.write(text);
    v691.close();
};

/*
function getxh()
{
  var x = false;
  try {
    x=new ActiveXObject("Msxml2.XMLHTTP");
  }
  catch (e) {
    try {
      x=new ActiveXObject("Microsoft.XMLHTTP");
    }
    catch (E) {
      x=false;
    }
  }
  if (!x && document.createElement)
    {
      try {
	x = new XMLHttpRequest();
      } catch (e) {
	x=false;
      }
    }
  return x;
}
*/

function encPar(wide)
{
    var narrow= encUtf8(wide);
    var enc= '';
    for (var i= 0; i<narrow.length; i++)
    {
	if (encPar_OK.indexOf(narrow.charAt(i))==-1)
	    enc= enc+encHex2(narrow.charCodeAt(i));
	else
	    enc= enc+narrow.charAt(i);
    }
    return enc;
}

var encPar_OK= 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*@-_./';

function encHex2(v)
{
    return '%'+encHex2_DIGITS.charAt(v>>>4)+encHex2_DIGITS.charAt(v&0xF);
}
var encHex2_DIGITS= '0123456789ABCDEF';

function encUtf8(wide) {
    var c, s;
    var enc= '';
    var i= 0;
    while(i<wide.length){
	c= wide.charCodeAt(i++);
	// handle UTF-16 surrogates
	if (c>=0xDC00 && c<0xE000) continue;
	if (c>=0xD800 && c<0xDC00){
	    if (i>=wide.length) continue;
	    s= wide.charCodeAt(i++);
	    if (s<0xDC00 || c>=0xDE00) continue;
	    c= ((c-0xD800)<<10)+(s-0xDC00)+0x10000;
	}
	// output value
	if (c<0x80) enc+=
	    String.fromCharCode(c);
	else if (c<0x800) enc+=
	    String.fromCharCode(0xC0+(c>>6),0x80+(c&0x3F));
	else if (c<0x10000) enc+=
	    String.fromCharCode(0xE0+(c>>12),0x80+(c>>6&0x3F),0x80+(c&0x3F));
	else enc+=
	    String.fromCharCode(0xF0+(c>>18),0x80+(c>>12&0x3F),
        0x80+(c>>6&0x3F),0x80+(c&0x3F));
    }
    return enc;
}

/*
try
{
    encodeURIComponent('a');
}
catch (e)
{
    encodeURIComponent = encPar;
}
*/


/*
function F3IncTimeoutCounter(){
    F3TimeOutCounter++;
    window.status = '  ';
};

function fgt(name) {
    var item;
    if (document.getElementById)
        item=document.getElementById(name);
    else
        item=document.all[name];
    return item;
}

function SendToLink2(v456, v457, v458, v459){
    var container;
    var containerName = 'F'+ FrameNb;
    FrameNb++;
    if (!F3PushURL) return;
    document.body.insertAdjacentHTML( 'afterBegin', '<span id=\"SPAN' + containerName + '\"></span>' );
    var span = fgt("SPAN" + containerName);
    var html = '<iframe name=\"' + containerName + '\" src=\"javascript:void;\"></iframe>';
    span.innerHTML = html;
    span.style.display = 'none';
    container = window.frames[containerName];
    var doc = container.document;
    doc.open();
    doc.write('<html><body>');
    doc.write('<form name=\"go\" method=\"post\" target=\"\" action=\"'+v459+'\">');
    doc.write('<input type=\"hidden\" name=\"v654\">');
    doc.write('<input type=\"hidden\" name=\"v645\">');
    doc.write('<input type=\"hidden\" name=\"v465\">');
    doc.write('</form></body></html>');
    doc.close();
    fgt(v654).value=v456;
    fgt(v645).value=v457;
    fgt(v465).value=v458;
    doc.forms['go'].submit();
};

function F3Watchdog()
{
    // console.log("F3Watchdog: " + F3TimeOutCounter + " " + F3WatchdogCounter);
  if (F3TimeOutCounter >= F3WatchdogCounter)
    {
      var x = getxh();
      if (x)
	SendToLink('', '', '', F3PushURL);
      else
	window.frames['Lisp1'].document.location.replace(F3PullURL);F3WatchdogCounter=F3TimeOutCounter;
    }
};
*/
/*
function SendToLink(v456, v457, v458, v459){
// v458 is set by F3SetPushURL()
    var x = getxh();
    window.ble = x;
    if (!x) return SendToLink2(v456, v457, v458, v459);

    //if (!F3PushURL)
    //    return;
    console.log("456: "+v456+" 457: "+v457+" 458-val: "+ v458+ " 459 "+ v459);
    // x.open("POST", v459, true);
    var msg;
    var msg_stringify;
    msg = new Object();
    msg.type = "id";
    msg.value = F3LinkId;
    msg_stringify = JSON.stringify(msg);
    // alert ("ws.sends " + msg_stringify);
    ws.send(msg_stringify);

    msg.type = "link";
    msg.value = v459;
    msg_stringify = JSON.stringify(msg);
    // alert("ws.sends: " + msg_stringify);
    ws.send(msg_stringify);

    ws.onmessage = function (evt){
        console.log(evt.data);
        eval(evt.data);
    };
    return null;

   // x.onreadystatechange = function() {
   //     if(x.readyState == 4) {
   //         eval(x.responseText);
   //         x.onreadystatechange = function() {};
   //     }
   // }
   // x.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
   //  x.send ("v654="+v456+"&v645="+v457+"&v465="+encodeURIComponent(v458)+"&v564=1");
};
*/
