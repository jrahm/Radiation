import vim
import subprocess

def handle_line( line, out ) :
    head = line[:line.find(":")] ;
    tail = line[line.find(":")+1:] ;

    if head == "q":
        ev = get_default(tail, None) ;
        if ev == None:
            out.write("\n")
        else:
            out.write("." + ev + "\n") ;
    elif head == "e":
        ev = vim.eval(tail) ;
        out.write(ev + "\n") ;
    elif head == "c" :
        vim.command(tail)
    elif head == "p":
        pass

def get_default( name, default ) :
    ec = vim.eval("exists('"+name+"')") ;
    if ec == "0" :
        return default
    return vim.eval(name) ;

def radiate(filename, filetype, _):
    servername = vim.eval("v:servername") ;

    radiation_binary = get_default("g:radiation_binary", "radiation") ;

    print ("radiation binary: " + radiation_binary)
    if servername == "":
        # There is no servername. We do everything sequential
    
        proc = subprocess.Popen([radiation_binary, filename, filetype], stdout=subprocess.PIPE, stdin=subprocess.PIPE) ;

        stin = proc.stdin
        stout = proc.stdout

        line = proc.stdout.readline()
        while line:
            handle_line( line[:-1], stin ) ;
            line = proc.stdout.readline()
    else:
        proc = subprocess.Popen([radiation_binary, filename, filetype, servername]) ;
