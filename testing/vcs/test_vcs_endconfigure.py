import vcs, sys

class FakeConfigurator(object):
    def __init__(self):
        self.detached = False

    def detach(self):
        self.detached = True

x = vcs.init()

fake = FakeConfigurator()

x.configurator = fake
x.close()

if x.configurator is not None:
    print "x.close() did not end configuration"
    sys.exit(1)

if fake.detached == False:
    print "x.close() did not detach configurator"
    sys.exit(1)

fake = FakeConfigurator()
x.configurator = fake
x.onClosing(None)

if x.configurator is not None:
    print "x.onClosing did not end configuration"
    sys.exit(1)

if fake.detached == False:
    print "x.onClosing() did not detach configurator"
    sys.exit(1)

sys.exit(0)