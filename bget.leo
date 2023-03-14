<?xml version="1.0" encoding="utf-8"?>
<!-- Created by Leo: http://leoeditor.com/leo_toc.html -->
<leo_file xmlns:leo="http://leoeditor.com/namespaces/leo-python-editor/1.1" >
<leo_header file_format="2"/>
<globals/>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="caminhante.20230226160006.1"><vh>@settings</vh>
<v t="caminhante.20230226160006.2"><vh>@data abbreviations</vh></v>
<v t="caminhante.20230226160006.3"><vh>@outline-data tree-abbreviations</vh></v>
<v t="caminhante.20230226160006.4"><vh>nodeActions</vh>
<v t="caminhante.20200604232050.1"><vh>@cmd *</vh></v>
<v t="caminhante.20211012181130.1"><vh>@cp *</vh></v>
<v t="caminhante.20211113172701.1"><vh>@files Makefile</vh></v>
<v t="caminhante.20210501142945.2"><vh>@mkdir *</vh></v>
<v t="caminhante.20210501142945.3"><vh>@mv file://*</vh></v>
<v t="caminhante.20211012180812.1"><vh>@mv *</vh></v>
<v t="caminhante.20210905232208.1"><vh>@rm *</vh></v>
<v t="caminhante.20210501142945.4"><vh>@rmdir /*/</vh></v>
<v t="caminhante.20200618203115.1"><vh>@run|@run .* [X]</vh></v>
</v>
</v>
<v t="caminhante.20230314155659.2"><vh>@path .</vh>
<v t="caminhante.20230314155735.4"><vh>/.git/</vh></v>
<v t="caminhante.20230314155736.1"><vh>@clean Makefile</vh></v>
<v t="caminhante.20230314170703.1"><vh>@edit .gitignore</vh></v>
<v t="caminhante.20221211153038.1"><vh>@file bget.c</vh></v>
<v t="caminhante.20230314155735.1"><vh>bget.leo</vh></v>
<v t="caminhante.20230314155735.3"><vh>LICENSE</vh></v>
<v t="caminhante.20230314155735.2"><vh>README.md</vh></v>
</v>
</vnodes>
<tnodes>
<t tx="caminhante.20200604232050.1">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

if c.isChanged(): c.save()

path = c.getNodePath(c.p)
g.os.chdir(path)

command = ''.join(c.p.h.split('@cmd ')[1:])
command = "cat &lt;&lt;'EOFEOFEOFEOFEOFEOF' | bash\n"+command+'\nEOFEOFEOFEOFEOFEOF'
g.es(c.p.h)
c.p.b += execute(command)

c.save()
</t>
<t tx="caminhante.20200618203115.1">@language python
import subprocess

# def getpath (p):
    # dict = c.scanAllDirectives(p)
    # d = dict.get("path")
    # if p.isAnyAtFileNode():
        # filename = p.anyAtFileNodeName()
        # filename = g.os_path_join(d,filename)
        # if filename:
            # d = g.os_path_dirname(filename)
    # if d is None:
        # return ""
    # else:
        # return g.os_path_normpath(d)

# if c.isChanged(): c.save()

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

w = c.frame.body.wrapper
s1,s2 = w.getSelectionRange()

if s1 == s2:
    code = c.p.b
else:
    linebr = c.p.b.find('\n',s2)
    if linebr == -1: linebr = len(c.p.b)
    code = c.p.b[s1:s2]

g.es(c.p.h)

command = "cat &lt;&lt;'EOFEOFEOFEOFEOFEOF' | bash\n"+code+'\nEOFEOFEOFEOFEOFEOF'

# if s1 == s2:
    # c.p.b += '\n' + execute(command)
# else:
    # c.p.b = c.p.b[0:linebr] + '\n' + execute(command) + c.p.b[linebr:]

cpc = c.p.copy()
novoNo = cpc.insertAsLastChild()
novoNo.h = c.p.h + ' ['+code+']'
novoNo.b = execute(command)
c.redraw()

c.save()
w.setSelectionRange(s1,s2)
</t>
<t tx="caminhante.20210501142945.2">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

path = ''.join(c.p.h.split('@mkdir ')[1:])
cmd = "mkdir -p -- '" + path + "'"
g.es(c.p.h)
execute(cmd)
c.p.h = '/'+path+'/'
c.p.b = '@path ' + path
</t>
<t tx="caminhante.20210501142945.3">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

path = ''.join(c.p.h.split('@mv file://')[1:])
cmd = "mv -v -- '" + path + "' ./"
g.es(c.p.h)
g.es(execute(cmd))
</t>
<t tx="caminhante.20210501142945.4">import subprocess
import shutil

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

ps = c.p.parents()
for par in ps:
    break
path = c.getNodePath(par)
g.os.chdir(path)

path = ''.join(c.p.h.split('@rmdir /')[1:]).split('/')[0]
shutil.rmtree(path)
c.p.b = 'deleted'
</t>
<t tx="caminhante.20210905232208.1">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

filename = ''.join(c.p.h.split('@rm ')[1:])
cmd = 'rm -vf -- "' + filename + '"'
g.es(c.p.h)
g.es(execute(cmd))
</t>
<t tx="caminhante.20211012180812.1">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

path = ''.join(c.p.h.split('@mv ')[1:])
cmd = "mv -v -- '" + path + "' ./"
g.es(c.p.h)
g.es(execute(cmd))
</t>
<t tx="caminhante.20211012181130.1">import subprocess

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

path = c.getNodePath(c.p)
g.os.chdir(path)

path = ''.join(c.p.h.split('@cp ')[1:])
cmd = "cp -v -- '" + path + "' ./"
g.es(c.p.h)
g.es(execute(cmd))
</t>
<t tx="caminhante.20211113172701.1">@language python
import subprocess
import re

def execute (cmd):
    return subprocess.run(cmd,shell=True,universal_newlines=True,stderr=subprocess.STDOUT,stdout=subprocess.PIPE).stdout

if c.isChanged(): c.save()

path = c.getNodePath(c.p)
g.os.chdir(path)

cpc = c.p.copy()
novoNo = cpc.insertAfter()
novoNo.h = 'make return'
novoNo.b = execute('make')
c.redraw()
</t>
<t tx="caminhante.20230226160006.1">@tabwidth -2
</t>
<t tx="caminhante.20230226160006.2"></t>
<t tx="caminhante.20230226160006.3"></t>
<t tx="caminhante.20230226160006.4">@language python
</t>
<t tx="caminhante.20230314155659.2"></t>
<t tx="caminhante.20230314155735.1"></t>
<t tx="caminhante.20230314155735.2"></t>
<t tx="caminhante.20230314155735.3"></t>
<t tx="caminhante.20230314155735.4">@path .git</t>
<t tx="caminhante.20230314155736.1">@tabwidth 4
CFLAGS := -std=gnu11 -Wall -Wextra -Werror -Wfatal-errors -Wno-unused-function -Wno-trigraphs -fno-common -fomit-frame-pointer -fstrict-aliasing

all: bget.so

bget.so: bget.c
	gcc $(CFLAGS) "$&lt;" -shared -o "$@"</t>
</tnodes>
</leo_file>
