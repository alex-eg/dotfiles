#/bin/bash

pid_file=~/.syncthing-pid
screen_name="syncthing_screen"

is_syncthing_running() {
    if [[ ! -f "${pid_file}" ]]; then
        return 1
    else
        local pid=$(<"${pid_file}")
        if ! kill -0 "$pid" >/dev/null 2>&1; then
            rm -f "${pid_file}"
            return 1
        fi
    fi
    return 0
}

if ! is_syncthing_running; then
    if screen -ls "${screen_name}" >/dev/null 2>&1; then
        screen -S "${screen_name}" -X kill
    fi
    screen -DmS "${screen_name}" syncthing &
    screen_pid=$!
    shell_pid=''
    while [[ -z "${shell_pid}" ]]; do
        shell_pid=$(ps h --ppid "${screen_pid}" -opid 2>/dev/null)
    done
    pid=''
    while [[ -z "${pid}" ]]; do
        pid=$(ps h --ppid "${shell_pid}" -opid 2>/dev/null)
    done
    echo "${pid}" > "${pid_file}"
    echo "Syncthing started successfully, pid is ${pid}"
fi
