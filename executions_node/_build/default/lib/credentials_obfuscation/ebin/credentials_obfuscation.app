{application,credentials_obfuscation,
             [{description,"Helper library that obfuscates sensitive values in process state"},
              {vsn,"3.4.0"},
              {licenses,["MPL2.0","ASL2"]},
              {links,[{"GitHub",
                       "https://github.com/rabbitmq/credentials-obfuscation"}]},
              {registered,[]},
              {mod,{credentials_obfuscation_app,[]}},
              {applications,[kernel,stdlib,crypto]},
              {env,[{enabled,true}]},
              {modules,[credentials_obfuscation,credentials_obfuscation_app,
                        credentials_obfuscation_pbe,
                        credentials_obfuscation_sup,
                        credentials_obfuscation_svc]}]}.