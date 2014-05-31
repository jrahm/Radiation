import vim
import subprocess

# Called when there is a line from
# radiation to respond to
def handle_line(line, out):
    # the head and the tail of the line
    head = line[:line.find(":")] ;
    tail = line[line.find(":")+1:] ;

    if head == "q":
        # commands starting with a `q:' are asking
        # for the value of a variable
        ev = get_default(tail, None) ;

        # Variables have a dot before the value
        # if they are defined, or just an empty line
        # if not defined.
        if ev == None:
            out.write("\n")
        else:
            out.write("." + ev + "\n") ;

    elif head == "e":
        # commands starting with an `e:' are telling
        # Vim to evaluate something.
        ev = vim.eval(tail) ;
        out.write(ev + "\n") ;

    elif head == "c":
        # commands starting with a `c:' are telling vim to
        # execute a command.
        vim.command(tail)

    elif head == "p":
        pass

# Tries to get the value of a variable, but if that
# variable does not exist, then a default value is
# returned instead
def get_default(name, default):
    ec = vim.eval("exists('"+name+"')") ;
    if ec == "0":
        return default
    return vim.eval(name) ;

# The main function which is called by the VimScript
# This will test the name of the server and decide if
# the external program should be executed as a sequential
# program or as a server program.
def radiate(filename, filetype, _):

    # get the servername
    servername = vim.eval("v:servername") ;
    # get the radiaition binary assuming the default is
    # `radiation'
    radiation_binary = get_default("g:radiation_binary", "radiation") ;
    if servername == "":
        # There is no servername. This means radiation will
        # run sequentially; blocking the user from any further
        # interaction until the binary halts
        proc = subprocess.Popen([radiation_binary, filename, filetype], stdout=subprocess.PIPE, stdin=subprocess.PIPE) ;

        stin = proc.stdin
        stout = proc.stdout
        line = stout.readline()
        
        # Read the lines from radiation
        # and communicate back.
        while line:
            handle_line(line[:-1], stin) ;
            line = stout.readline()

    else:
        # This is an asynchronous program. There is no need to
        # help the process directly as it has a straight shot to
        # Vim itself.
        #
        # This call will not wait for the process to end.
        proc = subprocess.Popen([radiation_binary, filename, filetype, servername]) ;
