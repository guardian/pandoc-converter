import { GuEc2App } from '@guardian/cdk';
import { AccessScope } from '@guardian/cdk/lib/constants';
import type { GuStackProps } from '@guardian/cdk/lib/constructs/core';
import { GuStack } from '@guardian/cdk/lib/constructs/core';
import { GuCname } from '@guardian/cdk/lib/constructs/dns';
import type { App } from 'aws-cdk-lib';
import { Duration } from 'aws-cdk-lib';
import { InstanceClass, InstanceSize, InstanceType } from 'aws-cdk-lib/aws-ec2';

export class PandocConverter extends GuStack {
	converter: GuEc2App;
	cname: GuCname;

	constructor(scope: App, id: string, props: GuStackProps) {
		super(scope, id, props);

		const domainName = 'pandoc-converter.gutools.co.uk';

		this.converter = new GuEc2App(this, {
			access: {
				scope: AccessScope.PUBLIC,
			},
			app: 'pandoc-converter',
			applicationPort: 9482,
			certificateProps: {
				domainName,
			},
			imageRecipe: 'pandoc-converter-ubuntu-jammy-x86',
			instanceType: InstanceType.of(InstanceClass.T3, InstanceSize.NANO),
			monitoringConfiguration: { noMonitoring: true },
			scaling: {
				minimumInstances: 1,
			},
			userData: {
				distributable: {
					executionStatement:
						'converterPath=$(sudo nix-store --import < /pandoc-converter/pandoc-converter.closure | grep PandocConverter); $converterPath/bin/PandocConverter',
					fileName: 'pandoc-converter.closure',
				},
			},
		});

		this.cname = new GuCname(this, 'PandocConverterDNS', {
			app: this.converter,
			ttl: Duration.hours(1),
			domainName,
			resourceRecord: this.converter.loadBalancer.loadBalancerDnsName,
		});
	}
}
