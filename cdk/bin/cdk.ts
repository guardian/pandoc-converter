import "source-map-support/register";
import { GuRoot } from "@guardian/cdk/lib/constructs/root";
import { PandocConverter } from "../lib/pandoc-converter";

const app = new GuRoot();
new PandocConverter(app, "PandocConverter-euwest-1-CODE", { stack: "content-api", stage: "CODE", env: { region: "eu-west-1" } });
new PandocConverter(app, "PandocConverter-euwest-1-PROD", { stack: "content-api", stage: "PROD", env: { region: "eu-west-1" } });
