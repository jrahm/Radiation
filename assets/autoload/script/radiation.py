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
else:
    TEMP = os.path.join("/tmp", "radiation", os.getenv("USER"))

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

def radiate(filename, filetype):
    global g_running_process

    open_log()

    radiation_source(filename, True) # source the cached version if it exists

    radiation_binary = get_default("g:radiation_binary", "radiation")

    # first, read the required variables from the binary so we know
    # what the background process will need to complete the radiation
    argv = [radiation_binary, filename, filetype, "--requires"]
    debug("argv: %s" % argv)
    proc = subprocess.Popen(argv, stdout=subprocess.PIPE)

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
    g_running_process = subprocess.Popen(argv)
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

def radiation_source(filename, is_cache=False):
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
            os.rename(newfilename, newfilename + ".cache")
