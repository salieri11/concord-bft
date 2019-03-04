-- This script creates a new cockroach database named 'helen' and also
-- creates a new admin user named 'helen_admin' for that database. As of
-- now contracts management services and profile(user) management services
-- need this database.
-- The table creation part for profile(user) managment services is done
-- in this script (because those services are developed using hibernate and
-- so we don't have to directly deal with SQL quries). However, the
-- contract management services were developed earlier and hence they
-- directly deal with SQL queries using JDBC. Hence, contract management
-- table creation is not done here

-- Create a helen database
CREATE DATABASE IF NOT EXISTS helen;

-- Allow helen_admin all access to helen database
GRANT ALL ON DATABASE helen TO helen_admin;

-- switch to helen database
use helen;


-- Profile schema creation
-- Below queries are taken from hibernate logs and will have to be modified
-- if we add a new persistent entity or update existing entity.

-- Sequence used by hibernate for assigning auto generated ID
create sequence if not exists hibernate_sequence start 1 increment 1;

-- Consoritums entity
create table if not exists consortiums (consortiumid UUID not null,
consortium_name varchar(255),
consortium_type varchar(255), primary key (consortiumid));

-- Organizations entity
create table if not exists organizations (organizationid UUID not null,
organization_name varchar(255),
primary key (organizationid));

-- Users entity
create table if not exists users (userid UUID not null, email
varchar(255) UNIQUE, first_name varchar(255), last_login int8, last_name
varchar(255), name varchar(255) not null, password varchar(255) not
null, role varchar(255) not null, consortium_consortiumid UUID not
null, organization_organizationid UUID not null, primary key (userid),
foreign key (consortium_consortiumid) references consortiums, foreign
key (organization_organizationid) references organizations);

-- Blockchain entity
create table if not exists blockchains (id UUID not null unique,
consortium_consortiumid UUID not null, ip_list varchar,  rpc_urls varchar,
rpc_certs varchar,
foreign key (consortium_consortiumid) references consortiums,
primary key (id));

	-- User Agreements
	-- This one type is an exception to the uuid id rule.  Currently, there is only one agreement.
	-- This will need to change
create table if not exists agreements (id int8 not null unique,
type varchar(255) not null, first_name varchar(255),
last_name varchar(255), company varchar(255), accepted_on int8,
accepted boolean not null default false,
content text not null);

insert into agreements (id, type, accepted, content) values (
	1, 'PRE-RELEASE SERVICE OFFERING', false, 'VMWARE PRE-RELEASE SERVICE OFFERING
TERMS OF SERVICE
By accessing a Pre-Release Service Offering, you agree to these Terms of Service. If you do not agree to these Terms of Service, you must not access or use the Pre-Release Service Offering. If you are entering into these Terms of Service on behalf of an organization, you represent to us that you have the authority to bind that entity. Capitalized terms used in these Terms of Service are defined in Section 1 (Definitions), below.

These Terms of Service govern your access to and use of the Pre-Release Service Offering. You may use the Pre-Release Service Offering only for non-production purposes. We provide the Pre-Release Service Offering to you strictly “AS IS”, free of charge, and without any warranty or indemnification of any kind.

These Terms of Service take effect when you click “I accept” or similar button or check box presented to you as part of the sign-up process or when you first use a Pre-Release Service Offering, whichever is earlier, and will remain in effect until terminated as specified in these Terms of Service.

1.	Definitions.
1.1	“Confidential Information” means non-public technical, business or other information or materials disclosed or otherwise made available by one party to the other that are in tangible form and labeled “confidential” or the like. Our Confidential Information includes:  (a) Pre-Release Service Offering; (b) Performance Data; (c) Login Credentials; and (d) any information or materials relating to VMware products or services (including any trade secrets, know-how, inventions, techniques, processes, and algorithms embodied in a Pre-Release Service Offering, pricing, product or service roadmaps, strategic marketing plans, product or service designs, and technical requirements and workflows), whether or not marked as such. Your Confidential Information does not include Your Content.
1.2	“Documentation” means the printed or online written reference materials provided to you by VMware with Your use of the Pre-Release Service Offering, including instructions, evaluation testing guidelines, and end user guides.
1.3	“Feedback Sessions” means meetings conducted by VMware in which you participate, whether in person or by telephone or via the Internet, including customer summits, customer roundtables, design workshops, and discussions.
1.4	“Intellectual Property Rights” means all worldwide intellectual property rights, including copyrights, trademarks, service marks, trade secrets, patents, patent applications, and moral rights laws, whether registered or unregistered.
1.5	“Login Credentials” mean any passwords, authentication keys or security credentials that enable your access to and management of a Pre-Release Service Offering.
1.6	“Pre-Release Service Offering” means a pre-release version of a software-as-a-service made available to You pursuant to these Terms of Service.
1.7	“Terms of Service” means the terms and conditions as provided herein governing Your use of the Pre-Release Service Offering.
1.8	“Third Party Content” means third party data, service, content, software or applications.
1.9	“User” means any person who accesses Your Content or uses a Pre-Release Service Offering under your Login Credentials.
1.10	“VMware”, “we”, “us” or “our” means VMware, Inc.
1.11	“You” and “Your” means you individually or the organization that you represent.
1.12	“Your Content” means any and all applications, files, information, data or other content uploaded to or published or displayed through a Pre-Release Service Offering by you and your Users. It does not include Performance Data or Technical Data.
2.	Pre-Release Service Offering.
2.1	Generally. These Terms of Service govern (a) your access to and use of any Pre-Release Service Offering, and (b) your participation in Feedback Sessions. We may deliver any Pre-Release Service Offering to you with the assistance of our affiliates, licensors, and service providers. You must comply with all laws, rules and regulations applicable to your use of the Pre-Release Service Offering.
2.2	Access and Use.
2.2.1	You may access and use any Pre-Release Service Offering solely for purposes of internal testing and evaluation, and to provide feedback to us. You may use the Documentation provided with a Pre-Release Service Offering solely in support of your authorized use of the Pre-Release Service Offering.
2.2.2	To access a Pre-Release Service Offering, we may require you to register with VMware and set up an authorized account with Login Credentials. We are not required to confirm your registration until you provide all required information. When VMware accepts your request to access a Pre-Release Service Offering, we will deliver the corresponding Login Credentials to you by email to the address associated with your account.
2.2.3	You must keep your Login Credentials confidential. If you set up an account for an organization, you must require that all Users of that account keep their Login Credentials confidential. You must keep your registration information accurate, complete and current as long as you access or use a Pre-Release Service Offering. You are responsible for any use that occurs under your Login Credentials, including any activities by you, your employees, contractors or agents. If you believe an unauthorized person has gained access to your Login Credentials, you must notify us as soon as possible. We will not be responsible for any unauthorized access to or use of your account.
2.2.4	We may monitor the overall performance and stability of the infrastructure hosting the Pre-Release Service Offering. You must not block or interfere with that monitoring. If we reasonably believe a problem with a Pre-Release Service Offering may be attributable to your use of a Pre-Release Service Offering or to Your Content, you must cooperate with us to identify the source of and to resolve that problem.
2.3	Other VMware Service Offerings or Software. As part of your access to or use of a Pre-Release Service Offering, you may receive access to additional VMware applications, which may be subject to separate terms. If so, those separate terms will prevail over these Terms of Service as to your access to and use of that application.
2.4	Third Party Content. As part of your access to or use of a Pre-Release Service Offering, you may receive access to Third Party Content, which may be subject to separate terms. If so, those separate terms will prevail over these Terms of Service as to your use of the Third Party Content. Third Party Content is available “AS IS”, without indemnification, support, warranty, or condition of any kind. You are responsible for reviewing, accepting, and complying with any third party terms of use or other restrictions applicable to the Third Party Content. If those terms are unavailable through the Pre-Release Service Offering, the terms will be added as an appendix to these Terms of Service. We will not provide any support for Third Party Content unless otherwise provided in the Third Party Terms. We reserve the right to suspend or terminate Third Party Content at any time.
2.5	Evaluation Feedback. As consideration for access to and use of a Pre-Release Service Offering, you will, from time to time, as we may reasonably request, provide feedback (including comments and suggestions) to us, and only to us, concerning (a) information that we may disclose in Feedback Sessions, and (b) the functionality and performance of the Pre-Release Service Offering. We may use feedback and other information regarding the Pre-Release Service Offering that you provide to us, through any means, to improve or enhance our products, in our sole discretion. You hereby grant us an exclusive, perpetual, irrevocable, royalty-free, worldwide right and license, with the right to sublicense, to use, reproduce, disclose, distribute, display, perform, modify, prepare derivative works of, and otherwise exploit that feedback without restriction in any manner now known or in the future conceived, and to make, have made, use, sell, offer to sell, import and export any product or service that incorporates the feedback, and you will have no claim on any resulting Intellectual Property Rights.
2.6	Open Source Software. You may have access to open source software if you use a Pre-Release Service Offering. Any open source software provided to you is made available under the applicable open source license, which may be provided under an appendix to these Terms of Service.
2.7	Verifying Compliance. We have the right to verify your compliance with these Terms of Service. If we seek to verify that compliance, you must provide information or other materials we reasonably request to assist in the verification. If we have reason to believe that you or a User has breached these Terms of Service, then we or our designated agent may review your use of the Pre-Release Service Offering, including your account, Your Content, and records, to verify your compliance with these Terms of Service.
3.	Your Content.
3.1	General. You are solely responsible for Your Content. You are responsible for protecting the security of Your Content, including any access to Your Content that you might provide to your employees, customers, or other third parties.
3.2	Data Protection; Security.
3.2.1	By accepting these Terms of Service, you warrant and represent that you understand that a Pre-Release Service Offering is not intended to be used with any data of a commercial or sensitive nature. You must not submit any personal data to a Pre-Release Service Offering, including (a) personal data within the meaning of that term given in the Regulation 2016/679 (the EU General Data Protection Regulation) or (b) protected health information within the meaning of the United States Health Insurance Portability and Accountability Act.
3.2.2	You are responsible for ensuring that the security of the Pre-Release Service Offering is appropriate for your intended use of that Pre-Release Service Offering and Your Content. You are responsible for (a) protecting the security of Your Content, including without limitation, any access you might provide to Your Content by your employees or other third parties, and in transit to and from the Pre-Release Service Offering; (b) properly configuring the Pre-Release Service Offering so that it is suitable for your use; (c) backing up Your Content to the extent required; (d) encrypting Your Content to the extent required; and (e) complying with any laws or regulations applicable to Your Content and your use of the Pre-Release Service Offering.
3.2.3	As between you and us, you are responsible for any losses or other consequences arising from your failure to encrypt or to back up Your Content, including any loss of or damage to any data.
4.	Acceptable Use.
4.1	Use Restrictions
4.1.1	You and any Users accessing an Pre-Release Service Offering through you may not:  (a) use the Pre-Release Service Offering:  (i) in a way prohibited by law, regulation, governmental order or decree; (ii) to violate the rights of others; (iii) to try to gain unauthorized access to, test the vulnerability of, or disrupt the Pre-Release Service Offering, or any other service, device, data, account, or network; (iv) to spam or to distribute malware; (v) in a way that could harm the Pre-Release Service Offering or impair anyone else’s use of it; (vi) in a way intended to work around the Pre-Release Service Offering’s technical limitations or usage limits, if any; or (vii) in any application or situation where failure of the Pre-Release Service Offering could lead to the death or serious bodily injury of any person, or to severe physical or environmental damage; (b) disclose, provide, or disseminate to any third party, including your partners, affiliates or subsidiaries, in any manner, the Pre-Release Service Offering or any information that we provide to you regarding the Pre-Release Service Offering; or (c) use any part of a Pre-Release Service Offering or any information provided to you by us for your production purpose. You must ensure that your Users comply with the terms of these Terms of Service. You agree that if you become aware of any violation by one of your Users, you will terminate that User’s access immediately.
4.1.2	You will not, and will not permit any third party to:  (a) disable, interfere with, disrupt, or circumvent any aspect of a Pre-Release Service Offering, including the integrity or performance of the Pre-Release Service Offering, or Third Party Content or data provided through a Pre-Release Service Offering; or (b) rent, resell, copy, modify, create derivative works of, translate, or sublicense a Pre-Release Service Offering.  For the sake of clarity, the Pre-Release Service Offering and all performance data and test results, including benchmark test results (collectively “Performance Data”), relating to a Pre-Release Service Offering are the Confidential Information of VMware, and are to be treated in accordance with the terms of Section 9 (“Confidential Information”) of these Terms of Service. Accordingly, you will not publish or disclose to any third party any Performance Data relating to a Pre-Release Service Offering.
5.	IP Ownership.
5.1	Ownership. As between you and us, we and our licensors own and retain all right, title and interest in and to any Pre-Release Service Offering, including all improvements, enhancements, modifications and derivative works thereof, and all Intellectual Property Rights therein. This includes any information that we collect and analyze in connection with any Pre-Release Service Offering such as usage patterns and other information to improve and evolve our software products and service offerings. Your rights to use any Pre-Release Service Offering are limited to those expressly granted in these Terms of Service. No other rights with respect to any Pre-Release Service Offering or any related Intellectual Property Rights are implied. No license or right under any Intellectual Property Right is granted under these Terms of Service or by any disclosure of Confidential Information except as expressly set forth in these Terms of Service.
5.2	Ownership of Your Content. As between you and us, you and your Users retain all right, title and interest, and all Intellectual Property Rights, in and to Your Content. Our rights to access and use Your Content are limited to those expressly granted in these Terms of Service. No other rights with respect to Your Content or any related Intellectual Property Rights are implied.
6.	Term and Termination.
6.1	Term of Pre-Release Service Offering. The term of these Terms of Service, and your rights with respect to a Pre-Release Service Offering, will be in effect until we turn off access to the Pre-Release Service Offering or if such rights are terminated earlier as permitted under these Terms of Service.
6.2	Termination. Either party may terminate these Terms of Service or your rights with respect to a Pre-Release Service Offering at any time for any reason or for no reason by providing the other party written notice of that termination, and that termination will be effective immediately upon delivery of the notice. For example, if you breach your confidentiality obligations as set forth in Section 9 (“Confidential Information”) of these Terms of Service, these Terms of Service and your rights with respect to a Pre-Release Service Offering will automatically terminate without notice, and we may immediately revoke your access to the Pre-Release Service Offering.
6.3	Effect of Termination.
6.3.1	Upon termination of these Terms of Service or your rights to a Pre-Release Service Offering:  (a) all rights granted to you under these Terms of Service in connection with that Pre-Release Service Offering, including your ability to access any of Your Content stored in the Pre-Release Service Offering, will immediately terminate; and (b) you must promptly discontinue all use of the Pre-Release Service Offering and delete or destroy (or, in the case of electronic data, use commercially reasonable efforts to delete or render practically inaccessible) any of our Confidential Information provided to you in connection with these Terms of Service and the Pre-Release Service Offering.
6.3.2	Sections 1 (Definitions), 2.6 (Evaluation Feedback), 2.8 (Open Source Software), 4 (Acceptable Use), 5 (IP Ownership), 6 (Term and Termination), 7 (Disclaimer), 8 (Limitation of Liability), 9 (Confidential Information), 10 (Technical Data; Log Files), and 11 (General) of these Terms of Service will survive the termination of these Terms of Service.
7.	Disclaimer.
7.1	TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, WE AND OUR LICENSORS AND SERVICE PROVIDERS DISCLAIM ALL WARRANTIES, CONDITIONS, AND OTHER TERMS, WHETHER EXPRESS, IMPLIED, OR STATUTORY, INCLUDING ANY IMPLIED WARRANTIES AND CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, NON-INFRINGEMENT AND ANY WARRANTIES AND CONDITIONS ARISING FROM COURSE OF DEALING OR COURSE OF PERFORMANCE, RELATING TO ANY PRE-RELEASE SERVICE OFFERING, ANY DOCUMENTATION, AND ANY MATERIALS OR SERVICES FURNISHED OR PROVIDED TO YOU UNDER THE AGREEMENT. WE AND OUR LICENSORS AND SERVICE PROVIDERS DO NOT WARRANT, COVENANT OR GIVE ANY CONDITION OR TERM THAT A PRE-RELEASE SERVICE OFFERING WILL BE UNINTERRUPTED OR FREE FROM DEFECTS, OR THAT A PRE-RELEASE SERVICE OFFERING WILL MEET (OR IS DESIGNED TO MEET) YOUR BUSINESS REQUIREMENTS OR ANY SERVICE LEVELS.
7.2	You acknowledge that: (a) any Pre-Release Service Offering may contain features currently under development, (b) VMware has not promised or guaranteed to you that any Pre-Release Service Offering will be announced or made available to anyone in the future, (c) VMware has no express or implied obligation to you to announce or introduce any Pre-Release Service Offering, (d) VMware may elect not to introduce a product similar to or compatible with any Pre-Release Service Offering, and (e) any version number (if any) referenced is subject to change and does not in any way represent VMware’s commitment to release any product or service in the future. Specifically, any Pre-Release Service Offering may contain features, functionality or modules that may not be included in the services that we may offer on a commercial basis, or in the generally available commercial version of a Pre-Release Service Offering, if released, or that may be marketed separately for additional fees. Therefore, you expressly acknowledge and agree that any research or development that you perform regarding any Pre-Release Service Offering or any product or service associated with any Pre-Release Service Offering is done entirely at your own risk.
8.	Limitation of Liability.
8.1	Generally. TO THE MAXIMUM EXTENT PERMITTED BY LAW, IN NO EVENT WILL WE OR OUR LICENSORS OR SERVICE PROVIDERS BE LIABLE FOR ANY LOST PROFITS OR BUSINESS OPPORTUNITIES, LOSS OF USE OF THE SERVICES, SOFTWARE OR SYSTEMS WHICH ARE PART OF ANY PRE-RELEASE SERVICE OFFERING, LOSS OF REVENUE, LOSS OF GOODWILL, BUSINESS INTERRUPTION, LOSS OF DATA, OR ANY INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL LOSSES IN CONNECTION WITH THESE TERM OF SERVICE UNDER ANY THEORY OF LIABILITY, WHETHER BASED IN CONTRACT, TORT, NEGLIGENCE, PRODUCT LIABILITY, OR OTHERWISE. IN ANY EVENT, OUR LIABILITY IN CONNECTION WITH THESE TERMS OF SERVICE WILL NOT, IN ANY EVENT, REGARDLESS OF WHETHER A CLAIM IS BASED IN CONTRACT, TORT, STRICT LIABILITY OR OTHERWISE, EXCEED FIVE DOLLARS ($5.00 USD OR EQUIVALENT IN LOCAL CURRENCY) REGARDLESS OF WHETHER WE HAVE BEEN ADVISED OF THE POSSIBILITY OF THOSE DAMAGES.
8.2	Further Limitations. Our licensors and service providers will have no liability of any kind under these Terms of Service. You may not bring a claim under these Terms of Service more than eighteen (18) months after the cause of action arises.
9.	Confidential Information.
9.1	Protection. A party may use Confidential Information of the other party solely to exercise its rights and perform its obligations under these Terms of Service or as otherwise permitted under these Terms of Service. Each party may disclose the Confidential Information of the other party only to the employees or contractors of the recipient party who have a need to know the Confidential Information for purposes of these Terms of Service and who are under a duty of confidentiality no less restrictive than each party’s duty under these Terms of Service (“Representatives”). Each recipient party is responsible for any acts or omissions of its Representatives that, if taken by the recipient party, would constitute a breach of these Terms of Service. All Confidential Information disclosed under these Terms of Service will remain the property of the disclosing party. Each party will use reasonable care to protect the confidentiality of the other party’s Confidential Information. Regardless of any expiration or termination of these Terms of Service, you must meet your obligations with respect to Confidential Information provided by VMware under these Terms of Service for five years after receipt of that Confidential Information (except for source code, which must be kept in confidence for perpetuity).
9.2	Exceptions. The recipient party’s obligations under Section 9.1 with respect to any Confidential Information will terminate if the recipient party can show by written records that the information: (a) was already rightfully known to the recipient party at the time of disclosure by the other party; (b) was disclosed to the recipient party by a third party who had the right to make the disclosure without any confidentiality restrictions; (c) is, or through no fault of the recipient party has become, generally available to the public; or (d) was independently developed by the recipient party without access to or use of the discloser’s Confidential Information. The recipient party may disclose Confidential Information to the extent the disclosure is required by law or regulation or the listing rules of any stock exchange. The recipient party will provide the other party reasonable advance notice, when practicable and to the extent permitted by law, and will take reasonable steps to contest and limit the scope of any required disclosure to the minimum required by the law or regulation. Breach of this Section 9 by the recipient party may cause the disclosing party substantial harm for which monetary damages are an insufficient remedy. Accordingly, each party agrees that the other party may apply for injunctive relief in respect of any threatened or actual breach of this Section 9.
10.	Technical Data; Log Files.
10.1	Technical Data. We may collect, process and store technical and related information about your device, system, application, peripherals, and your use of a Pre-Release Service Offering, including internet protocol address, hardware identification, operating system, application software, peripheral hardware, number of active plugins and software development kits, the successful installation and launch of an Pre-Release Service Offering, the amount of computing and storage resources purchased or consumed, user counts, third party licenses or services consumed, and other Pre-Release Service Offering usage statistics (collectively, “Technical Data”). We use Technical Data to provide the Pre-Release Service Offering; for internal statistical and analytical purposes; to provision updates; to develop and improve VMware products and services; to track and manage our infrastructure, network, storage and software; and for capacity planning, troubleshooting, and other forecasting purposes. We may share Technical Data with our affiliates and third party service providers for these purposes and as otherwise required by applicable law.
11.	General.
11.1	Assignment. You may not assign or otherwise transfer any of your rights or obligations under these Terms of Service, in whole or in part, by operation of law or otherwise, without our prior written consent. Any attempted assignment or transfer without that consent will be void. Subject to these limits, these Terms of Service will bind and inure to the benefit of the parties and their respective successors and assigns.
11.2	Notices. Any notice from us to you under these Terms of Service will be delivered by email to the email address associated with your account or by posting to the community page associated with the Pre-Release Service Offering, except as otherwise set forth in these Terms of Service. Please direct legal notices or other correspondence to VMware, Inc., 3401 Hillview Avenue, Palo Alto, California 94304, United States of America, Attention: Legal Department.
11.3	Modifications. We reserve the right to modify, suspend, or terminate a Pre-Release Service Offering (or any part thereof), either temporarily or permanently, at any time or from time to time, with or without prior notice to you. We may change the terms of these Terms of Service, or of your access to the Pre-Release Service Offering. It is your responsibility to regularly check the applicable Pre-Release Service Offering website for updates. Your continued use of a Pre-Release Service Offering after the effective date of any modification to the Pre-Release Service Offering or these Terms of Service will be deemed acceptance of the modified Pre-Release Service Offering or these Terms of Service, as applicable.
11.4	Waiver. Any waiver of any provision of these Terms of Service must be in writing and signed by the waiving party to be effective. The waiver of a breach of any provision of these Terms of Service will not constitute a waiver of any other provision or of any later breach.
11.5	Severability. If any provision of these Terms of Service is held to be invalid or unenforceable, the provision will be enforced to the maximum extent permissible so as to effect the intent of the parties, and the remaining provisions of these Terms of Service will remain in force.
11.6	Compliance with Laws; Export Control. Each party will comply with all laws applicable to the actions contemplated by these Terms of Service. You acknowledge that any Pre-Release Service Offering provided to you by VMware pursuant to these Terms of Service is of United States origin, is provided subject to the U.S. Export Administration Regulations (including “deemed export” and “deemed re-export” regulations), and may be subject to the export control laws of other applicable territories. You represent and warrant that (a) you are not, and are not acting on behalf of, (1) any person who is a citizen, national, or resident of, or who is controlled by the government of any country to which the United States has prohibited export transactions; or (2) any person or entity listed on the U.S. Treasury Department list of Specially Designated Nationals and Blocked Persons, or the U.S. Commerce Department Denied Persons List or Entity List (3) any person found on the UK Designated Persons list; (b) you will not permit a Pre-Release Service Offering to be used for any purposes prohibited by law, including any prohibited development, design, manufacture or production of missiles or nuclear, chemical or biological weapons; (c) Your Content will not be classified or listed on the United States Munitions list, contain defense articles, defense services or contain ITAR-related data; or technology related to the Munitions List found in the UK Strategic Export Controls List; (d) Your Content will not require an export license and is not restricted from export to, or from, any VMware global resource or personnel under applicable export control laws; and (e) you are not subject, either directly or indirectly, to any order issued by any agency of the United States government, revoking or denying, in whole or in part, your United States export privileges.  You will notify VMware immediately if you become subject to any such order.
11.7	Force Majeure. We will not be liable for any delay or failure to perform any obligations under these Terms of Service due to any cause beyond our reasonable control, including acts of God, labor disputes or other industrial disturbances, systemic electrical, telecommunications or other utility failures, earthquakes, storms or other elements of nature, blockages, embargoes, riots, acts or orders of government, acts of terrorism or war.
11.8	Construction. The headings of sections of these Terms of Service are for convenience and are not for use in interpreting these Terms of Service. As used in these Terms of Service, the word “including” means “including but not limited to.”
11.9	Governing Law and Forum. These Terms of Service are governed by the laws of the State of California, without regard to its choice of law principles. The United Nations Convention for the International Sale of Goods shall not apply.
11.10	Third Party Rights. Other than as expressly set forth in these Terms of Service, these Terms of Service does not create any rights for any person who is not a party to it, and no person who is not a party to these Terms of Service may enforce any of its terms or rely on any exclusion or limitation contained in it.
11.11	Order of Precedence. Other than as expressly set forth in these Terms of Service, the terms of these Terms of Service will supersede and control over any conflicting or additional terms and conditions of any other document for any Pre-Release Service Offering.
11.12	Entire Agreement. These Terms of Service, as may be revised by us, are the entire agreement of the parties regarding its subject matter. These Terms of Service supersede all prior or contemporaneous communications, understandings and agreements, whether written or oral, between the parties regarding its subject matter.
')
on conflict (id)
do nothing;

-- keystore entity
create table if not exists keystores (address varchar(40) not null,
wallet text not null, user_userid UUID, foreign key (user_userid) references users,
primary key (address));

-- contracts --
create sequence if not exists contract_sequence start with 1 increment 1;

create table if not exists contracts (id UUID not null, contract_id text not null, version_name text not null,
address text, sourcecode text, bytecode text, metadata text, owner text,
sequence_number integer default nextval('contract_sequence'),
blockchain_id UUID not null,
primary key (id));

-- entity tables --
create table if not exists entity (
  created_id   bigserial,
  row_key      uuid   not null,
  column_name  varchar(64)  not null,
  version      int          not null,
  body         jsonb         not null, -- 64kb; native json in 10.2?
  user_id      uuid   not null,
  user_name    varchar(64)  not null,
  created_tms  timestamp    not null  default current_timestamp,
  primary key (created_id),
  unique (row_key, version)
);

create table if not exists entity_history (
  created_id   bigserial,
  row_key      uuid   not null,
  column_name  varchar(64)  not null,
  version      int          not null,
  body         jsonb         not null, -- 64kb; native json in 10.2?
  user_id      uuid   not null,
  user_name    varchar(64)  not null,
  created_tms  timestamp    not null  default current_timestamp,
  primary key (created_id),
  unique (row_key, version)
);

-- relationships should be kept in entities' bodies. link table is a performance optimization for queries
create table if not exists link (
  from_row  uuid  not null,
  to_row    uuid  not null,
  unique (from_row, to_row)
  );



