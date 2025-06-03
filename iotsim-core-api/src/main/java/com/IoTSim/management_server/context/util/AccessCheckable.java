package com.IoTSim.management_server.context.util;

import com.IoTSim.management_server.context.user.model.User;

public interface AccessCheckable {
    boolean hasAccess(User user);
}
