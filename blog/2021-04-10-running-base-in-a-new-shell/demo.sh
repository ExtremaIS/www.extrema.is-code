#!/usr/bin/env bash

_demo_load_env () {
  local var
  while IFS=$'\n' read -r var ; do
    case "${var}" in
      BASH_* | FUNCNAME | GROUPS | cmd | val )
        ;;
      DEMO_ENV | var )
        ;;
      * )
        declare -p "${var}" | sed -e 's/\t/\\t/' -e '$! s/$/\\n/' | tr -d '\n'
        echo
        ;;
    esac
  done < <(declare -p | grep '^declare' | sed 's/^declare -[^ ]* \([^=]\+\).*$/\1/')
  alias -p
}

if [ -z "${DEMO_ENV_SER}" ] ; then
  if [ "${BASH_SOURCE[0]}" == "${0}" ] ; then
    echo "usage: . ${0}" >&2
    exit 2
  fi

  declare -a DEMO_ENV
  readarray -t DEMO_ENV < <(_demo_load_env)
  unset -f _demo_load_env

  /usr/bin/env \
    DEMO_ENV_SER="$(declare -p DEMO_ENV)" \
    bash --init-file "${BASH_SOURCE[0]}"

  unset DEMO_ENV
  return 0
fi

unset -f _demo_load_env

_demo_restore_env () {
  local defcmd envcmd var
  for rstcmd in "${DEMO_ENV[@]}" ; do
    if [[ "${rstcmd}" =~ ^declare ]] ; then
      defcmd="${rstcmd#declare -* }"
      var="${defcmd%%=*}"
      envcmd="$(declare -p "${var}" 2>/dev/null)"
      if [[ -z "${envcmd}" || "${envcmd}" =~ ^declare\ -[^r\ ]*\  ]] ; then
        echo "${rstcmd}"
      fi
    else
      echo "${rstcmd}"
    fi
  done
}

eval "${DEMO_ENV_SER}"
while IFS=$'\n' read -r rstcmd ; do
  eval "${rstcmd}"
done < <(_demo_restore_env)
unset -f _demo_restore_env
unset DEMO_ENV DEMO_ENV_SER
