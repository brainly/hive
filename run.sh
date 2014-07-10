#!/bin/bash

cat <<EOF
Brainly Hive start script
EOF

usage()
{
cat <<EOF
Usage:
  ./run.sh [-h|--help] [-e|--env=prod] [-u|--ulimit=1024] [-p|--plugins=plugins/] [-c|--config=etc/hive.json] [-s|--schema=etc/schema/]

Options:
  -h, --help                Display this help
  -e, --env <dev|prod>      Application environment, defaults to "prod"
  -u, --ulimit              Define limit of opened descriptors
  -p, --plugins             Path to plugins directory
  -c, --config              Path to configuration file
  -s, --schema              Path to config file schemas

Examples:
  * Start Hive in development environment
    ./run.sh --env=dev
  * Start Hive in production environment
    ./run.sh
  * Start Hive with other than default configurtion path 
    ./run.sh -e dev --config=/etc/hive/config.json 

EOF
return 0
}

start_dev()
{
exec erl +K true \
    $(escript priv/prep_hive.erl $@) \
    -pa ebin edit deps/*/ebin \
    -s lager \
    -s hive start_dev $@

return 0
}

start_prod()
{
exec erl +K true \
     $(escript priv/prep_hive.erl $@) \
    -noinput \
    -pa ebin edit deps/*/ebin \
    -s lager \
    -s hive start $@
}

# Set defaults for options
env=prod
ulimit=1024
plugins=plugins/
config=etc/hive.json
schema=etc/schema/

# Read options
while getopts ":he:u:p:c:s:" opt; do
    case $opt in
        h) usage && exit 0;;
        e)
            env=$OPTARG
            ;;
        u)
            ulimit=$OPTARG
            ;;
        p)
            plugins=$OPTARG
            ;;
        c)
            config=$OPTARG
            ;;
        s)
            schema=$OPTARG
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

cat <<EOF
Starting Hive with the following parameters:
  - env: $env
  - ulimit: $ulimit
  - plugins: $plugins
  - config: $config
  - schema: $schema
EOF

escript priv/test_config.erl $plugins $schema $config

if [ $? -ne 0 ]; then
    exit 1
fi

ulimit -n $ulimit

case $env in
    dev)
        start_dev $plugins $schema $config
        ;;
    prod)
        start_prod $plugins $schema $config
        ;;
    *)
        usage
        exit 1
        ;;
esac
