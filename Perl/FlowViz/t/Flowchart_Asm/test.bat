@echo off
copy D:\zwx\svn\agent\unisimu\Perl\FlowViz\lib\GraphViz\Flowchart\Asm.pm E:\perl\site\lib\graphviz\flowchart\
perl compile.t
perl basic.t
del   tmp*