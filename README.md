## ASL.Impl

### Phase 0

I will implement according the following plan:
1. [JsonPath](https://github.com/json-path/JsonPath) - without support of filter expressions;
1. Fail state - full impl;
1. Succeed state - full impl;
1. Choice state - full impl;
1. Wait state - full impl;
1. Pass state - full impl;
1. Task state - without support of "TimeoutSeconds", "TimeoutSecondsPath", "HeartbeatSeconds", "HeartbeatSecondsPath" properties;
1. Parallel state - full impl;
1. Map state - without support of "ItemReader", "ItemBatcher", "ResultWriter", "MaxConcurrency", "ToleratedFailurePercentage", "ToleratedFailureCount" properties;
1. State machine - without support of "TimeoutSeconds" property;
1. Intrinsic Functions - don't support;
1. Predefined Error Codes - support only States.ALL.