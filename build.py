import json
import os

from eluthia.decorators import chmod, copy_files, copy_folder, file, git_clone, empty_folder, build_and_unpack_erlang_release
from eluthia.defaults import control
from eluthia.functional import pipe
from eluthia.py_configs import deb822, nginx

DOMAIN = 'chat.robin.au'

PUBLIC_PORT = 80

FRONTEND_ENV = {
    'WS_PROTOCOL': 'ws',
    'WS_HOST': DOMAIN,
    'WS_PORT': PUBLIC_PORT,
}

@chmod(0o755)
@file
def postinst(full_path, package_name, apps):
    return f'''\
        #!/bin/bash
        systemctl daemon-reload

        # Enable and start the service
        systemctl enable {package_name}
        systemctl restart {package_name} nginx
    '''

@file
def systemd_service(full_path, package_name, apps, machine_name, machines):
    server_env = {
        'SERVER_PORT': machines[machine_name]['port_by_name']['chat_server'],
    }
    environment_variables = '\n'.join(
        f"        Environment={variable}={value}"
        for variable, value in {**server_env, **apps[package_name]['env']}.items()).strip()

    return f'''\
        [Unit]
        Description=Chat Service
        After=network.target
        [Service]
        Type=simple
        User=ubuntu
        ExecStart=/home/ubuntu/system/chat_server/bin/erlchat_release foreground
        {environment_variables}
        [Install]
        WantedBy=multi-user.target
    '''

def nginx_conf(full_path, package_name, apps, machine_name, machines):
    server_port = machines[machine_name]['port_by_name']['chat_server']
    env_json = json.dumps(FRONTEND_ENV)
    env_init_location = ('location', '=', '/importEnv.js', (
        ('return', 200, f"'importEnv = () => ({env_json})'"),
    ))
    return (
        ('map', '$http_upgrade', '$connection_upgrade', (
            # Websocket related
            # https://www.nginx.com/blog/websocket-nginx/
            ('default', 'upgrade'),
            ("''", 'close'),
        )),
        ('server', (
            ('listen', PUBLIC_PORT),
            ('server_name', DOMAIN),
            ('charset', 'utf-8'),

            # root must be set at server level in order for "location = /" to work
            ('root', '/home/ubuntu/system/chat_front'),

            env_init_location,
            ('location', '/', (
                # Try $uri file or $uri/ directory or @backend
                ('try_files', '$uri', '$uri/', '@backend'),
            )),
            ('location', '@backend', (
                # Websocket backend
                ('proxy_pass', f"http://127.0.0.1:{server_port}"),
                ('proxy_http_version', '1.1'),
                ('proxy_set_header', 'Upgrade', '$http_upgrade'),
                ('proxy_set_header', 'Connection', '$connection_upgrade'),
                ('proxy_set_header', 'Host', '$host'),
            )),
        )),
    )

def get_package_tree(package_name, apps):
    return {
        'DEBIAN': {
            'postinst': postinst,
            'control': file(pipe(
                control,
                lambda d: {**d, 'Description': 'Chat Programme'},
                deb822)),
        },
        'home': {
            'ubuntu': {
                'system': {
                    'chat_front': copy_files(os.path.join(apps[package_name]['folder'], 'frontend'), [
                        'index.html',
                    ]),
                    'chat_server': build_and_unpack_erlang_release(os.path.join(apps[package_name]['_app_folder'], 'erlchat')),
                }
            },
        },
        'etc': {
            'systemd': {
                'system': {
                    f'{package_name}.service': systemd_service,
                },
            },
            'nginx': {
                'sites-enabled': {
                    package_name: file(pipe(
                        nginx_conf,
                        nginx)),
                },
            },
        },
    }
