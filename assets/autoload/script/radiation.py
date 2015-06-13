import vim
import hashlib
import subprocess
import platform
import os

RADIATION_DEBUG = 1
radiation_pydebug = None
g_running_process = None

if platform.system() == "Windows":
    TEMP = os.path.join(os.getenv("TEMP"), "radiation")
    g_windows = True
else:
    TEMP = os.path.join("/tmp", "radiation", os.getenv("USER"))
    g_windows = False

def debug(string):
    if radiation_pydebug:
        radiation_pydebug.write(string + "\n")

# Tries to get the value of a variable, but if that
# variable does not exist, then a default value is
# returned instead
def get_default(name, default):
    error_code = vim.eval("exists('"+name+"')")
    if error_code == "0":
        return default
    return vim.eval(name)

def open_log():
    global radiation_pydebug
    if RADIATION_DEBUG and not radiation_pydebug:
        radiation_pydebug = open('radiation_pydebug.log', 'w')

def close_log():
    global radiation_pydebug
    if radiation_pydebug:
        radiation_pydebug.close()
        radiation_pydebug = None

def runprocess(argv, capture_stdout):
    proc = None
    if g_windows:
        if capture_stdout:
            proc = subprocess.Popen(argv, stdout=subprocess.PIPE, creationflags=subprocess.SW_HIDE, stderr=open(os.devnull), shell=True)
        else:
            proc = subprocess.Popen(argv, stdout=open(os.devnull, 'w'), stderr=subprocess.STDOUT, creationflags=subprocess.SW_HIDE, shell=True)
    else:
        if capture_stdout:
            proc = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=open(os.devnull))
        else:
            proc = subprocess.Popen(argv, stdout=open(os.devnull, 'w'), stderr=subprocess.STDOUT)
            
    return proc

        

def radiate(filetype):
    global g_running_process

    # get the filename
    filename = vim.eval("expand('%')")

    open_log()
    debug("radiate: %s" % filename)

    radiation_source(filename, True) # source the cached version if it exists

    radiation_binary = get_default("g:radiation_binary", "radiation")

    # first, read the required variables from the binary so we know
    # what the background process will need to complete the radiation
    argv = [radiation_binary, filename, filetype, "--requires"]
    debug("argv: %s" % argv)
    proc = runprocess(argv, True);

    stout = proc.stdout
    needed_vars = stout.readlines()
    needed_vars = [i.strip() for i in needed_vars]

    # now we know the variables that Radiation must know about
    # to continue. We can now run the binary to parse it.
    debug("needed vars: %s" % needed_vars)

    new_args = [(var, get_default(var, None)) for var in needed_vars]
    new_args = [("%s=%s" % (k, v)) for (k, v) in new_args if v is not None]

    argv = [radiation_binary, filename, filetype] + new_args
    debug("argv: %s" % argv)
    g_running_process = runprocess(argv, False)
    debug("detach process")

    # close_log()

def kill_running():
    global g_running_process
    open_log()

    if g_running_process != None:
        debug("killing " + str(g_running_process.pid))

        g_running_process.terminate()
        g_running_process = None

    # close_log()

def radiation_source(filename=None, is_cache=False):
    if not filename:
        filename = vim.eval("expand('%')")
    hashm = hashlib.md5()
    hashm.update(filename)
    hashname = hashm.hexdigest()

    cache = ".cache" if is_cache else ""

    # the filename of the radiated content is now
    # stored in the file $TEMP/radiation_$(md5sum filename)_x.vim
    newfilename = "%s/radiation_%s_x.vim%s" % (TEMP, hashname, cache)
    debug("sourcing: " + newfilename)

    if os.path.isfile(newfilename):
        vim.command("source " + newfilename)
        if not is_cache:
            # move the sourced file to the cached file
            cachepath = newfilename + ".cache"
            if os.path.exists(cachepath):
                os.remove(cachepath)
            os.rename(newfilename, cachepath)
