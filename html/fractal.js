
function f85425(Tabs, Panes, nTab, BaseClassName, SelectedClassName, remove)
{
 if (!nTab)
   nTab = 0;
 else
   nTab = parseInt(nTab);
 var Tab;
 for (var i = 0; i < Tabs.length; i++)
  {
   Tab = Tabs[i];
   Tab.style.borderLeftStyle = "";
   Tab.style.borderRightStyle = "";
   Tab.className = BaseClassName;
   Panes[i].style.display = "none";
  }
 Tabs[nTab].className = SelectedClassName;
 Panes[nTab].style.display = "";
 if (remove)
  {
   Tab = Tabs[nTab+1];
   if (Tab) Tab.style.borderLeftStyle = "none";
   Tab = Tabs[nTab-1];
   if (Tab) Tab.style.borderRightStyle = "none";
  }
 event.returnValue = false;
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
  if (event) event.returnValue = false;
}

function f825h(name)
{
  var item;
  item=document.getElementById(name);
  if (item) item.style.display = "none";
  if (event) event.returnValue = false;
}

function f8252s(name) {f825h(name+'d');f825s(name);}
    
function f8252h(name) {f825h(name);f825s(name+'d');}
    
function f825foc(name)
{
  f825h(name);
  f825s(name+'d');
  fire_onclick(name, 0);
  if (event)
      event.returnValue = false;
}

var v684=3651;var v683=3651; var v686=''; var v688; var v685=''; var v687=0;  var FrameNb = 12; var v689; var v690;
function F6541(){v684++;window.status = '  ';};
function F5641(v856, v857){v686=v856;v689=v857;};
function F5614(v856){v688=v856;};
function F5164(v856){v687=v856;};
function F5146(v856){return v687<v856;};
function SendLink(v456, v457, v458)
{
  var container;
  var containerName = 'F'+ FrameNb;
  FrameNb++;
  if (!v688) return;
  document.body.insertAdjacentHTML( 'afterBegin', '<span id=\"SPAN' + containerName + '\"></span>' );
  var span = document.all("SPAN" + containerName);
  var html = '<iframe name=\"' + containerName + '\" src=\"javascript:void;\"></iframe>';
  span.innerHTML = html;
  span.style.display = 'none';
  container = window.frames[containerName];
  var doc = container.document;
  doc.open();
  doc.write('<html><body>');
  doc.write('<form name=\"go\" method=\"post\" target=\"\" action=\"'+v688+'\">');
  doc.write('<input type=\"hidden\" name=\"v654\">');
  doc.write('<input type=\"hidden\" name=\"v645\">');
  doc.write('<input type=\"hidden\" name=\"v465\">');
  doc.write('</form></body></html>');
  doc.close();
  doc.all.v654.value=v456;
  doc.all.v645.value=v457;
  doc.all.v465.value=v458;
  doc.forms['go'].submit();
};

function F6451()
{
  if (v684 == v683) 
    window.frames['Lisp1'].document.location.replace(v686);v683=v684;
};

function f854(name, id)
{
 var s = "";
 for (var i = 0; i < 25; i++)
   {var item=document.getElementById(name+'c'+i)
     if (item && item.checked)
	 s=s+'t';
     else
	 s=s+'n';
   }
 return id+'='+s;
}

function open1(url, dx, dy, item)
{
 if (event)
  {
   var x = event.screenX;
   var y = event.screenY;
  }
 v690 = window.open(url+'?link='+v689+'&item='+item,'pop','status=no,width='+dx+'px,height='+dy+'px,resizable=yes,scrollbars=yes');
};

function close()
{
 if (v690)
  {
   v690.close();
   v690 = null;
  }
};

function fire_onchange(ItemName, value){SendLink('4', ItemName, value);};
function fire_add(ItemName, value){SendLink('12', ItemName, value);};
function fire_onclick(ItemName, value){SendLink('8', ItemName, value);};
function fire_call(ItemName, value){SendLink('13', ItemName, value);};


function open2(text, dx, dy)
{
  var v690 = window.open('about:blank','pop','status=no,width='+dx+'px,height='+dy+'px,resizable=yes,scrollbars=yes');
  var v691 = v690.document;
  v691.open("text/html", "replace");
  v691.write(text);
  v691.close();
};

